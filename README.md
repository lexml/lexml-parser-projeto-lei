lexml-parser-projeto-lei
========================

Parser LexML de documentos normativos

Para compilar:
```bash
cd lexml-parser-projeto-lei
mvn compile -nsu
```

Para testar:
```bash
mvn test
```

Para rodar (interface linha de comando):
```bash
cd lexml-parser-projeto-lei
mvn -Ponejar package
java -jar target/lexml-parser-projeto-lei-VERSAO-onejar.jar [args....]
```

Nota 1: para alguns formatos de entrada, pode ser necessário o Abiword.

Nota 2: Mais detalhes no [Guia de Instalação](https://github.com/lexml/lexml-parser-projeto-lei/wiki)
