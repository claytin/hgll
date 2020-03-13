qualifiedIdentifier     = "qualifiedIdentifier"     =|>
    identifier # closure (t "." # identifier)
qualifiedIdentifierList = "qualifiedIdentifierList" =|>
    qualifiedIdentifier # closure (t "," # qualifiedIdentifier)


compilationUnit = "compilationUnit" =|>
      opt (opt (anotations) # t "package" # qualifiedIdentifier # t ";")
    # closure (importDeclaration) # closure (typeDeclaration)

importDeclaration = "importDeclaration" =|>
    t "import" # opt (t "static") # qualifiedIdentifier # opt (t ".*")

typeDeclaration = "typeDeclaration" =|> classOrInterfaceDeclaration <|> t ";"

classOrInterfaceDeclaration = "classOrInterfaceDeclaration" =|>
    closure (modifier) # (classDeclaration <|> interfaceDeclaration)

classDeclaration = "classDeclaration" =|>
    normalClassDeclaration <|> enumDeclaration

interfaceDeclaration = "interfaceDeclaration" =|>
    normalInterfaceDeclaration <|> annotationTypeDeclaration


normalClassDeclaration = "normalClassDeclaration" =|>
      t "class" # identifier # opt (typeParameters)
    # opt (t "extends" # ttype) # opt (t "implements" # typeList)
    # classBody

enumDeclaration = "enumDeclaration" =|>
    t "enum" # identifier # opt (t "implements" # typeList) # enumBody

normalInterfaceDeclaration = "normalInterfaceDeclaration" =|>
      t "interface" # identifier # opt (typeParameters)
    # opt (t "extends" # typeList) # interfaceBody

annotationTypeDeclaration = "annotationTypeDeclaration" =|>
    t "@" # t "interface" # identifier # annotationTypeBody


ttype = "ttype" =|> (basicType <|> referenceType) # closure (t "[" # t "]")

basicType = "basicType" =|> t "byte"
                        <|> t "short"
                        <|> t "char"
                        <|> t "int"
                        <|> t "long"
                        <|> t "float"
                        <|> t "double"
                        <|> t "boolean"

referenceType = "referenceType" =|>
      identifier # opt (typeArguments)
    # closure (t "." # identifier # opt (typeArguments))

typeArguments = "typeArguments" =|>
    t "<" # typeArgument # closure (t "," # typeArgument) # t ">"
typeArgument  = "typeArgument"  =|>
        referenceType
    <|> t "?" # opt ((t "extends" <|> t "super") # referenceType)


nonWildcardTypeArguments = "nonWildcardTypeArguments" =|>
    t "<" # typeList # t ">"

typeList = "typeList" =|> referenceType # closure (t "," # referenceType)

typeArgumentsOrDiamond = "typeArgumentsOrDiamond" =|>
    typeArguments <|> t "<" # t ">"

nonWildcardTypeArgumentsOrDiamond = "nonWildcardTypeArgumentsOrDiamond" =|>
    nonWildcardTypeArguments <|> t "<" # t ">"

typeParameters = "typeParameters" =|>
    t "<" # typeParameter # closure (t "," # typeParameter) # t ">"
typeParameter  = "typeParameter"  =|>
    identifier # opt (t "extends" # bound)

bound = "bound" =|> referenceType # closure (t "&" # referenceType)


modifier = "modifier" =|> annotation
                      <|> t "public"
                      <|> t "protected"
                      <|> t "private"
                      <|> t "static"
                      <|> t "abstract"
                      <|> t "final"
                      <|> t "native"
                      <|> t "synchronized"
                      <|> t "transient"
                      <|> t "volatile"
                      <|> t "strictfp"

annotations = "annotations" =|> annotation # closure (annotation)
annotation  = "annotation"  =|>
    t "@" # qualifiedIdentifier # opt (t "(" # opt (annotationElement) # t ")")

annotationElement = "annotationElement" =|> elementValuePairs <|> elementValue

elementValuePairs = "elementValuePairs" =|>
    elementValuePair # closure (t "," # elementValuePair)
elementValuePair  = "elementValuePair"  =|> identifier # t "=" # elementValue
elementValue      = "elementValue"      =|> annotation
                                        <|> expression1
                                        <|> elementValueArrayInitializer

elementValueArrayInitializer = "elementValueArrayInitializer" =|>
    t "{" # opt (elementValues) # opt (t ",") # t "}"

elementValues = "elementValues" =|>
    elementValue # closure (t "," # elementValue)


classBody = "classBody" =|> t "{" # closure (classBodyDeclaration) # t "}"

classBodyDeclaration = "classBodyDeclaration" =|>
        opt (t "static") # block
    <|> closure (modifier) # memberDecl
    <|> t ";"

memberDecl = "memberDecl" =|> methodOrFieldDecl
                          <|> t "void" # identifier # voidMethodDeclaratorRest
                          <|> identifier # constructorDeclaratorRest
                          <|> genericMethodOrConstructorDecl
                          <|> classDeclaration
                          <|> interfaceDeclaration

methodOrFieldDecl = "methodOrFieldDecl" =|>
    ttype # identifier # methodOrFieldRest
methodOrFieldRest = "methodOrFieldRest" =|>
        fieldDeclaratorsRest # t ";"
    <|> methodDeclaratorRest

fieldDeclaratorsRest = "fieldDeclaratorsRest" =|>
    variableDeclaratorRest # closure (t "," # variableDeclarator)

methodDeclaratorRest     = "methodDeclaratorRest" =|>
      formalParameters # closure (t "[" # t "]")
    # opt (t "throws" # qualifiedIdentifierList) # (block <|> t ";")
voidMethodDeclaratorRest = "voidMethodDeclaratorRest" =|>
      formalParameters
    # opt (t "throws" # qualifiedIdentifierList) # (block <|> t ";")

constructorDeclaratorRest = "constructorDeclaratorRest" =|>
    formalParameters # opt (t "throws" # qualifiedIdentifierList) # block

genericMethodOrConstructorDecl = "genericMethodOrConstructorDecl" =|>
    typeParameters # genericMethodOrConstructorRest
genericMethodOrConstructorRest = "genericMethodOrConstructorRest" =|>
        (ttype <|> t "void") # identifier # methodDeclaratorRest
    <|> identifier # constructorDeclaratorRest


interfaceBody = "interfaceBody" =|>
    t "{" # closure (interfaceBodyDeclaration) # t "}"

interfaceBodyDeclaration = "interfaceBodyDeclaration" =|>
        closure (modifier) # interfaceMemberDecl
    <|> t ";"

interfaceMemberDecl = "interfaceMemberDecl" =|>
        interfaceMethodOrFieldDecl
    <|> t "void" # identifier # voidInterfaceMethodDeclaratorRest
    <|> interfaceGenericMethodDecl
    <|> classDeclaration
    <|> interfaceDeclaration

interfaceMethodOrFieldDecl = "interfaceMethodOrFieldDecl" =|>
    ttype # identifier # interfaceMethodOrFieldRest
interfaceMethodOrFieldRest = "interfaceMethodOrFieldRest" =|>
        constantDeclaratorsRest # t ";"
    <|> interfaceMethodDeclaratorRest

constantDeclaratorsRest = "constantDeclaratorsRest" =|>
    constantDeclaratorRest # closure (t "," # constantDeclarator)
constantDeclaratorRest  = "constantDeclaratorRest"  =|>
    closure (t "[" # t "]") # t "=" # variableInitializer

constantDeclarator = "constantDeclarator" =|>
    identifier # constantDeclaratorRest

interfaceMethodDeclaratorRest = "interfaceMethodDeclaratorRest" =|>
      formalParameters # closure (t "[" # t "]")
    # opt (t "throws" # qualifiedIdentifierList)

voidInterfaceMethodDeclaratorRest = "voidInterfaceMethodDeclaratorRest" =|>
    formalParameters # opt (t "throws" # qualifiedIdentifierList)

interfaceGenericMethodDecl = "interfaceGenericMethodDecl" =|>
      typeParameters # (ttype <|> t "void") # identifier
    # interfaceMethodDeclaratorRest


formalParameters = "formalParameters" =|>
    t "(" # opt (formalParameterDecls) # t ")"

formalParameterDecls = "formalParameterDecls" =|>
    closure (variableModifier) # ttype # formalParameterDeclsRest

variableModifier = "variableModifier" =|> t "final" <|> annotation

formalParameterDeclsRest = "formalParameterDeclsRest" =|>
        variableDeclaratorId # opt (t "," # formalParameterDecls)
    <|> t "..." # variableDeclaratorId


variableDeclaratorId = "variableDeclaratorId" =|>
    identifier # closure (t "[" # t "]")


variableDeclarators = "variableDeclarators" =|>
    variableDeclarator # closure (t "," # variableDeclarator)
variableDeclarator  = "variableDeclarator"  =|>
    identifier # variableDeclaratorRest

variableDeclaratorRest = "variableDeclaratorRest" =|>
    closure (t "[" # t "]") # opt (t "=" # variableInitializer)

variableInitializer = "variableInitializer" =|> arrayInitializer <|> expression

arrayInitializer = "arrayInitializer" =|>
      t "{"
    # opt (variableInitializer # closure (t "," # variableInitializer)
                               # opt (t ","))
    # t "}"


block = "block" =|> t "{" # closure (blockStatement) # t "}"

blockStatement = "blockStatement" =|> localVariableDeclarationStatement
                                  <|> classOrInterfaceDeclaration
                                  <|> opt (identifier # t ":") # statement

localVariableDeclarationStatement = "localVariableDeclarationStatement" =|>
    localVariableDeclaration # t ";"

localVariableDeclaration = "localVariableDeclaration" =|>
    closure (variableModifier) # ttype # variableDeclarators

statement = "statement" =|>
        block
    <|> t ";" <|> identifier # t ":" # statement
    <|> statementExpression # t ";"
    <|> t "if" # parExpression # statement # opt (t "else" # statement)
    <|> t "assert" # expression # opt (t ":" # expression) # t ";"
    <|> t "switch" # parExpression # t "{" # switchBlockStatementGroups # t "}"
    <|> t "while" # parExpression # statement
    <|> t "do" # statement # t "while" # parExpression # t ";"
    <|> for # t "(" # forControl # t ")" # statement
    <|> t "break" # opt (identifier) # t ";"
    <|> t "continue" # opt (identifier) # t ";"
    <|> t "return" # opt (expression) # t ";"
    <|> t "throw" # expression # t ";"
    <|> t "synchronized" # parExpression # block
    <|> t "try" # block # (catches <|> opt (catches) # finally)
    <|> t "try" # resourceSpecification # block # opt (catches) # opt (finally)

statementExpression = "statementExpression" =|> expression


catches = "catches" =|> catchClause # closure (catchClause)

catchClause = "catchClause" =|>
      t "catch" # t "(" # closure (variableModifier) # catchType # identifier
    # t ")" # block

catchType = "catchType" =|>
    qualifiedIdentifier # closure (t "|" # qualifiedIdentifier)

finally = "finally" =|> t "finally" # block

resourceSpecification = "resourceSpecification" =|>
    t "(" # resources # opt (t ";") # t ")"

resources = "resources" =|> resource # closure (t ";" # resource)
resource  = "resource"  =|>
      closure (variableModifier) # referenceType # variableDeclaratorId # t "="
    # expression


switchBlockStatementGroups = "switchBlockStatementGroups" =|>
    closure (switchBlockStatementGroup)
switchBlockStatementGroup  = "switchBlockStatementGroup"  =|>
    switchLabels # blockStatements

switchLabels = "switchLabels" =|> switchLabel # closure (switchLabel)
switchLabel  = "switchLabel"  =|>
        t "case" # (expression <|> enumConstantName) # t ":"
    <|> t "default" # t ":"

enumConstantName = "enumConstantName" =|> identifier


forControl = "forControl" =|>
        forVarControl
    <|> forInit # t ";" # opt (expression) # t ";" # opt (forUpdate)

forVarControl = "forVarControl" =|>
      closure (variableModifier) # ttype # variableDeclaratorId
    # forVarControlRest

forVarControlRest = "forVarControlRest" =|>
        forVariableDeclaratorsRest # t ";" # opt (expression)
                                            # t ";" # opt (forUpdate)
    <|> t ":" # expression

forVariableDeclaratorsRest = "forVariableDeclaratorsRest" =|>
    opt (t "=" # variableInitializer) # closure (t "," # variableDeclarator)

forInit   = "forInit"   =|> forUpdate
forUpdate = "forUpdate" =|> statementExpressions

statementExpressions = "statementExpressions" =|>
    statementExpression # closure (t "," # statementExpression)


expression = "expression" =|>
    expression1 # opt (assignmentOperator # expression1)

assignmentOperator = "assignmentOperator" =|>
        t "="
    <|> t "+=" <|> t "-=" <|> t "*=" <|> t "\="
    <|> t "&=" <|> t "|="
    <|> t "^=" <|> t "%="
    <|> t "<<=" <|> t ">>=" <|> t ">>>="

expression1 = "expression1" =|> expression2 # opt (expression1Rest)

expression1Rest = "expression1Rest" =|>
    t "?" # expression # t ":" # expression1

expression2 = "expression2" =|> expression3 # opt (expression2Rest)

expression2Rest = "expression2Rest" =|> closure (infixOp # expression3)
                                    <|> t "instanceof" # ttype


infixOp = "infixOp" =|> t "||" <|> t "&&" <|> t "|" <|> t "&"
                    <|> t "==" <|> t "!="
                    <|> t "<" <|> t ">" <|> t "<=" <|> t ">="
                    <|> t "<<" <|> t ">>" <|> t ">>>"
                    <|> t "+" <|> t "-" <|> t "*" <|> t "/" <|> t "%" <|> t "^"

expression3 = "expression3" =|>
        prefixOp # expression3
    <|> t "(" # (expression <|> ttype) # t ")" # expression3
    <|> primary # closure (selector) # closure (postfixOp)

prefixOp = "prefixOp" =|> t "++" <|> t "--"
                      <|> t "!" <|> t "~"
                      <|> t "+" <|> t "-"

postfixOp = "postfixOp" =|> t "++" <|> t "--"


primary = "primary" =|>
        literal
    <|> parExpression
    <|> t "this" # opt (arguments)
    <|> t "super" # superSiffix
    <|> t "new" # creator
    <|> nonWildcardTypeArguments
     #  (explicitGenericInvocationSuffix <|> t "this" # arguments)
    <|> qualifiedIdentifier # opt (identifierSuffix)
    <|> basicType # closure (t "[" # t "]") # t "." # t "class"
    <|> t "void" # t "." # t "class"


parExpression = "parExpression" =|> t "(" # expression # t ")"

arguments = "arguments" =|>
    t "(" # opt (expression # closure (t "," # expression)) # t ")"

superSiffix = "superSiffix" =|> arguments
                            <|> t "." # identifier # opt (arguments)

explicitGenericInvocationSuffix = "explicitGenericInvocationSuffix" =|>
        t "super" # superSiffix
    <|> identifier # arguments


creator = "creator" =|>
        nonWildcardTypeArguments # createdName # classCreatorRest
    <|> createdName # (classCreatorRest <|> arrayCreatorRest)

createdName = "createdName" =|>
      identifier # opt (typeArgumentsOrDiamond)
    # closure (t "." # identifier # opt (typeArgumentsOrDiamond))

classCreatorRest = "classCreatorRest" =|> arguments # opt (classBody)

arrayCreatorRest = "arrayCreatorRest" =|>
    t "[" # (  t "]" # closure (t "[" # t "]") # arrayInitializer
           <|> expression # t "]" # closure (t "[" # expression # t "]")
                                  # closure (t "[" # t "]"))


identifierSuffix = "identifierSuffix" =|>
        t "[" # (closure (t "[" # t "]") # t "." # t "class" <|> expression)
              # t "]"
    <|> arguments
    <|> t "." # (  t "class"
               <|> explicitGenericInvocation
               <|> t "this"
               <|> t "super" # arguments
               <|> t "new" # opt (nonWildcardTypeArguments) # innerCreator)

explicitGenericInvocation = "explicitGenericInvocation" =|>
    nonWildcardTypeArguments # explicitGenericInvocationSuffix

innerCreator = "innerCreator" =|>
    identifier # opt (nonWildcardTypeArgumentsOrDiamond) # classCreatorRest


selector = "selector" =|>
        t "." # (  identifier
               <|> explicitGenericInvocation
               <|> t "this"
               <|> t "super" # superSiffix
               <|> t "new" # opt (nonWildcardTypeArguments) # innerCreator)
    <|> t "[" # expression # t "]"


enumBody = "enumBody" =|>
      t "{"
    # opt (enumConstants) # opt (t ",") # opt (enumBodyDeclarations)
    # t "}"

enumConstants = "enumConstants" =|>
    enumConstant # closure (t "," # enumConstant)
enumConstant  = "enumConstant"  =|>
    opt (annotations) # identifier # opt (arguments) # opt (classBody)

enumBodyDeclarations = "enumBodyDeclarations" =|>
    t ";" # closure (classBodyDeclaration)

annotationTypeBody = "annotationTypeBody" =|>
    t "{" # closure (annotationTypeElementDeclaration) # t "}"

annotationTypeElementDeclaration = "annotationTypeElementDeclaration" =|>
    closure (modifier) # annotationTypeElementRest

annotationTypeElementRest = "annotationTypeElementRest" =|>
        ttype # identifier # annotationMethodOrConstantRest
    <|> classDeclaration
    <|> interfaceDeclaration
    <|> enumDeclaration
    <|> annotationTypeDeclaration

annotationMethodOrConstantRest = "annotationMethodOrConstantRest" =|>
        annotationMethodRest
    <|> constantDeclaratorsRest

annotationMethodRest = "annotationMethodRest" =|>
    t "(" # t ")" # opt (t "[" # t "]") # opt (t "default" # elementValue)
