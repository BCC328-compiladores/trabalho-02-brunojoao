# Documentacao Tecnica dos Modulos

Este documento detalha modulo por modulo como o compilador SL esta estruturado, quais tipos/funcoes centrais existem e como os dados fluem entre fases de compilacao e execucao.

## 1. Entrypoint e Orquestracao

### `workspace/app/Main.hs`

- **Papel:** ponto unico de entrada da CLI.
- **Responsabilidade:**
  - ler argumentos (`--lexer`, `--parser`, `--pretty`, `--semantic`, `--interp`);
  - carregar arquivo fonte;
  - invocar pipeline correta;
  - imprimir saida normal ou diagnosticos.
- **Funcoes-chave:**
  - `main`: roteia por flag;
  - fluxo `readFile -> scanTokens -> parseTokens`;
  - para `--semantic`: chama `analyzeProgram`;
  - para `--interp`: roda analise semantica e depois `runProgram`.
- **Conexao:** depende de Lexer, Parser, pretty-printers, Semantic e Interpreter.

## 2. Frontend Lexico

### `workspace/src/Token.hs`

- **Papel:** define o alfabeto tokenizado da linguagem.
- **Responsabilidade:** representar palavras-chave, operadores, literais e pontuacao com posicao (`line`, `col`).
- **Tipos-chave:**
  - `TokenType` (categoria);
  - `Token` (lexema + posicao).
- **Conexao:** consumido por `Lexer.x`, `Parser.y`, `TokenPretty.hs`.

### `workspace/src/Lexer.x` (fonte Alex)

- **Papel:** especificacao de regras lexicas.
- **Responsabilidade:** transformar texto em `[Token]`.
- **Geracao:** produz `workspace/src/Lexer.hs`.
- **Funcao-chave exportada:** `scanTokens`.
- **Conexao:** primeira fase do pipeline.

### `workspace/src/Lexer.hs` (gerado)

- **Papel:** automato lexer gerado pelo Alex.
- **Observacao:** nao deve ser editado manualmente; regenera via `alex src/Lexer.x -o src/Lexer.hs`.

## 3. Frontend Sintatico

### `workspace/src/Parser.y` (fonte Happy)

- **Papel:** gramatica LALR(1) da linguagem SL.
- **Responsabilidade:** transformar `[Token]` em `Program` (AST).
- **Recursos notaveis implementados:**
  - `forall` para parametros de tipo;
  - arrays, structs, funcoes, `if/else`, `while`, `for`, `return`, `print`;
  - `post-inc` (`i++`) e `.size` em arrays.
- **Geracao:** produz `workspace/src/Parser.hs` e `workspace/src/Parser.info`.
- **Funcao-chave exportada:** `parseTokens`.

### `workspace/src/Parser.hs` (gerado)

- **Papel:** parser gerado pelo Happy.
- **Observacao:** nao editar manualmente.

### `workspace/src/Parser.info` (gerado)

- **Papel:** relatorio de estados/reducoes da gramatica.
- **Uso:** depuracao de conflitos e precedencia.

## 4. AST e Modelagem de Linguagem

### `workspace/src/AST.hs`

- **Papel:** modelo canonicamente tipado da linguagem apos parsing.
- **Responsabilidade:**
  - representar programa completo (`Program`);
  - funcoes (`FunctionDecl`) com `funcTParams` (generics);
  - statements (`Stmt`) e expressoes (`Expr`);
  - tipos (`Type`) incluindo `TFun`, `TArray`, `TStruct`, `TNamed`.
- **Conexao:** modulo central compartilhado por parser, semantica, interpretador e pretty.

### `workspace/src/ASTPretty.hs`

- **Papel:** renderizacao de arvore AST em formato textual estruturado.
- **Uso:** suporta flag `--parser`.

### `workspace/src/CodePretty.hs`

- **Papel:** pretty-printer de codigo SL formatado a partir da AST.
- **Uso:** suporta flag `--pretty`.

### `workspace/src/TokenPretty.hs`

- **Papel:** formata stream de tokens para exibicao amigavel.
- **Uso:** suporta flag `--lexer`.

## 5. Tipos e Inferencia

### `workspace/src/TypeChecker/Types.hs`

- **Papel:** representacao de tipos semanticos internos.
- **Responsabilidade:** separar tipo de parse (`AST.Type`) do tipo de verificacao (`SType`).

### `workspace/src/TypeChecker/Unify.hs`

- **Papel:** unificacao de tipos.
- **Responsabilidade:** validar compatibilidade e resolver variaveis de tipo quando aplicavel.

### `workspace/src/TypeChecker/Infer.hs`

- **Papel:** inferencia de tipo em expressoes.
- **Responsabilidade:** calcular tipo de expressoes sem anotacao explicita e validar consistencia.

## 6. Ambientes (Estado Semantico)

### `workspace/src/Environment/SymbolTable.hs`

- **Papel:** tabela de simbolos com escopos empilhados.
- **Responsabilidade:** declaracao, lookup, entrada/saida de escopo.

### `workspace/src/Environment/TypeEnv.hs`

- **Papel:** ambiente de tipos de variaveis.
- **Responsabilidade:** mapear identificador -> tipo em escopos.

### `workspace/src/Environment/FuncEnv.hs`

- **Papel:** ambiente global de funcoes.
- **Responsabilidade:** assinatura de funcoes, aridade, tipos de parametros/retorno, generics.

### `workspace/src/Environment/StructEnv.hs`

- **Papel:** ambiente global de structs.
- **Responsabilidade:** definicoes de campos e validacao de acessos.

## 7. Diagnosticos

### `workspace/src/Errors/Diagnostic.hs`

- **Papel:** tipo estruturado para erros/avisos.
- **Campos principais:** codigo (`diagCode`), severidade, mensagem, posicao opcional, dica opcional.

### `workspace/src/Errors/Pretty.hs`

- **Papel:** renderizacao uniforme de diagnosticos.
- **Conexao:** usado por semantic checker e CLI para saida consistente.

## 8. Analise Semantica

### `workspace/src/Semantic/ScopeChecker.hs`

- **Papel:** regras de escopo.
- **Responsabilidade:** detectar uso de identificador nao declarado, sombras invalidas, disciplina de blocos.

### `workspace/src/Semantic/FunctionChecker.hs`

- **Papel:** regras de funcoes.
- **Responsabilidade:** validar declaracoes/chamadas, aridade, retorno e assinatura.

### `workspace/src/Semantic/StructChecker.hs`

- **Papel:** regras de structs/fields.
- **Responsabilidade:** validar campos, acessos e consistencia de tipos em estruturas.

### `workspace/src/Semantic/Analyzer.hs`

- **Papel:** orquestrador semantico principal.
- **Responsabilidade:**
  - construir ambientes globais;
  - rodar checks estruturais/escopo/funcoes;
  - inferir e validar tipos de expressoes/stmts;
  - acumular diagnosticos.
- **Funcao-chave:** `analyzeProgram`.

## 9. Interpretador

### `workspace/src/Interpreter/Value.hs`

- **Papel:** dominio de valores de runtime.
- **Responsabilidade:** representar `VInt`, `VFloat`, `VBool`, `VString`, arrays, structs e `VVoid`.

### `workspace/src/Interpreter/Builtins.hs`

- **Papel:** funcoes builtin (ex.: `print`).
- **Responsabilidade:** tabela de builtins e regra de aplicacao.

### `workspace/src/Interpreter/Eval.hs`

- **Papel:** maquina de execucao da AST.
- **Modelo:** estado mutavel controlado (`State`) + erro (`Except`), com escopos de variaveis.
- **Responsabilidade:**
  - avaliar expressoes;
  - executar statements (`if`, `while`, `for`, `return`);
  - chamar funcoes de usuario e builtins;
  - suportar recursao;
  - suportar arrays, structs, `post-inc` e `.size`.
- **Funcao-chave:** `runProgram`.

## 10. Suite de Testes

### `workspace/test/Main.hs`

- **Papel:** agregador de suites HUnit.

### `workspace/test/TestLexer.hs`

- **Cobertura:** tokenizacao e casos de sintaxe lexical.

### `workspace/test/TestParser.hs`

- **Cobertura:** parse valido/invalido e forma da AST gerada.

### `workspace/test/TestSemantic.hs`

- **Cobertura:** erros de escopo/tipo/funcao e casos validos (incluindo generics/for/array size no estado atual).

### `workspace/test/TestInterpreter.hs`

- **Cobertura:** execucao de programas fim-a-fim, saida e comportamento de controle de fluxo.

### `workspace/test/examples/*.sl`

- **Papel:** corpus de programas de referencia para parser, semantica e interpretador.

## 11. Dependencias e Build

### `workspace/compiler.cabal`

- **Papel:** manifesto de build.
- **Define:**
  - library + executable `compiler` + test-suite `compiler-test`;
  - dependencias (incluindo `mtl`, containers, base, HUnit etc.);
  - modulos expostos e compilados.

## 12. Fluxo de Dados entre Modulos

```text
Main.hs
  -> Lexer.scanTokens
  -> Parser.parseTokens
  -> AST (Program)
     -> Semantic.Analyzer.analyzeProgram
        -> (TypeChecker + Environment + Semantic.* + Errors)
     -> Interpreter.Eval.runProgram
        -> (Value + Builtins)
```

## 13. Gerado vs Manual

- **Gerados automaticamente:**
  - `workspace/src/Lexer.hs`
  - `workspace/src/Parser.hs`
  - `workspace/src/Parser.info`
- **Manuais (fonte real):** todos os demais modulos `.hs`, `Lexer.x`, `Parser.y`, testes e `Main.hs`.

## 14. Padrão de Extensao por Feature

Para adicionar uma feature de linguagem com seguranca:

1. Atualizar AST (`AST.hs`) se novo construtor for necessario.
2. Atualizar lexer (`Lexer.x`) e parser (`Parser.y`).
3. Estender inferencia/unificacao (`TypeChecker/*`).
4. Estender regras semanticas (`Semantic/*`).
5. Estender runtime (`Interpreter/Value.hs` e `Interpreter/Eval.hs`).
6. Expor na CLI se aplicavel (`app/Main.hs`).
7. Criar testes em `TestParser`, `TestSemantic`, `TestInterpreter`.

