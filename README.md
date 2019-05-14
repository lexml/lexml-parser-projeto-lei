lexml-parser-projeto-lei
========================

Parser LexML de documentos normativos

Para compilar:
```bash
cd lexml-parser-projeto-lei
mvn install -nsu
```

Para testar:
```bash
mvn test
```

Para rodar (interface linha de comando):
```bash
mvn scala:run -DmainClass=br.gov.lexml.parser.pl.fe.FECmdLine
```

Nota 1: para alguns formatos de entrada, pode ser necessário o Abiword.

Nota 2: Mais detalhes no [Guia de Instalação](https://github.com/lexml/lexml-parser-projeto-lei/wiki)
