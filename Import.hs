{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
 
pRoutes = [parseRoutes|

    / InicioR GET
    /page PageR GET 
    /cadastro CadastroR GET POST
    /listar ListarR GET
   /pessoa/#PessoaId PessoaR GET
   /depto DeptoR GET POST
  /usuario UsuarioR GET POST
  /professor ProfessorR GET POST
  /campus CampusR GET POST
  
   
   
   
|]
