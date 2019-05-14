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
mvn scala:run -DscalaClassName=br.gov.lexml.parser.pl.fe.FECmdLine
```
