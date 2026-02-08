[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/X15MPpfH)
# Trabalho prático de BCC328 - Construção de Compiladores I

## Suporte de Sintaxe .sl no Visual Studo Code

Para visualizar os arquivos `.sl` com destaque de sintaxe no VS Code, siga os passos abaixo:

### Instalação

1. Localize o arquivo `sl-recognizer-0.0.1.vsix` na pasta `/vs-code-extensions` dentro do projeto do trabalho.  
2. No terminal rode:

```bash
code --install-extension vs-code-extensions/sl-recognizer-0.0.1.vsix
```

## Iniciando o Container

Para iniciar o ambiente Docker:

```
docker-compose up -d
```
```
docker-compose exec sl bash
```

Isso abrirá um terminal dentro do container, já no diretório /workspace.

## Gerar o Relatório

```
pdflatex -output-directory=relatorio relatorio/relatorio.tex
```

## Compilar o projeto

```
cabal update
```

```
cabal install --installdir=. --overwrite-policy=always -v0
```

## Rodar Lexer

```
./compiler --lexer test/examples/ex01_factorial.sl
```

## Rodar Parser

```
./compiler --parser test/examples/ex01_factorial.sl
```

## Rodar Pretty

```
./compiler --pretty test/examples/ex01_factorial.sl
```

## Rodar Pretty

```
./compiler --semantic test/examples/ex01_factorial.sl
```

## Rodar Pretty

```
./compiler --interp test/examples/ex01_factorial.sl
```

## Rodar testes

Compila e executa todos os testes configurados:

```
cabal test
```

## Limpar os codigos compilados.

```
cabal clean
```