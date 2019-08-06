Extensões de Modelos de Regressão
=================================

Página com arquivos fontes para a página de Extensões de Modelos de
Regressão disponível em <http://web.leg.ufpr.br/ensino/EMR/>.

## Organização

  * `scripts/`: é para manter scripts R.
  * `slides/`: é para manter slides em Rmd, Rnw, tex, etc.
  * `tutoriais/`: é para manter tutorais em Rmd.
  * `config/`: é para arquivos de configuração da página.
    * `refs.bib`: contém as referências bibiográficas.

**ATENÇÃO**: apenas versionar os fontes essenciais. Arquivos gerados
pela compilação NÃO DEVEM SER VERSIONADOS.

## Construrir e publicar a página

Para renderizar a página, faça:
```sh
sh _build.sh site
```

Para transferir os arquivos para o servidor, faça:
```sh
sh _deploy.sh
```

Defina as variáveis de ambiente `PATAXOP`, `PATAXO`, e `WEBLEG` no
arquivo `.bashrc`.
