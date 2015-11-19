{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Import
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

--Formularios ---


formDepto :: Form Departamento
formDepto = renderDivs $ Departamento <$>
            areq textField "Nome" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident22",
                           fsLabel="Sigla",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","2")]} Nothing

formCampus :: Form Campus
formCampus = renderDivs $ Campus <$>
            areq textField "Campus" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident22",
                           fsLabel="Estado",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","2")]} Nothing


formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Nome" Nothing <*>
             areq intField "Idade" Nothing <*>
             areq doubleField "Salario" Nothing <*>
             areq (selectField dptos) "Depto" Nothing


dptos = do
       entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades


getInicioR :: Handler Html
getInicioR = defaultLayout [whamlet|
    <body bgcolor="#A9A9A9">
   <h1 align="center"> Faculdade de SpringField
       <p> Seja Bem Vindo a Faculdade de SpringField
          <p> Cursos Especializados para Personagens Ficticios em Geral.<br><br>
              <img src="http://i.imgur.com/vml0jJX.jpg"/><br>
                <a href=@{PageR}>Entrar no Site

|] 


getPageR :: Handler Html
getPageR = defaultLayout [whamlet|
    <body bgcolor="#A9A9A9">
   <h1 align="center"> 
       <p> Faculdade de SpringField
          <p> Endereço:19 Plympton Street.<br><br>
         <ul id="menu_fixo">
             <li><a href=@{UsuarioR}>Cadastro de Novo Aluno 
             <li><a href=@{ProfessorR}>Cadastro de Professor
             <li><a href=@{ProfessorR}>Cursos Disponiveis
             <li><a href=@{ProfessorR}>Disciplinas
             <li><a href=@{ProfessorR}>Grade Horaria
             <li><a href=@{ProfessorR}>Historico
             <li><a href=@{ProfessorR}>Coordenação
             <li><a href=@{ProfessorR}>Professores


|]
ww :: Widget
ww = toWidgetHead [hamlet|
<link rel="page" href=@{PageR} title="Sobre...">
|] >> toWidget[lucius|
    h1{

        color:red;
    }
 
|]


widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = [whamlet|
            <h1 align="center">
                Cadastro de #{y}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
|] >> toWidget[lucius|
    h1{

        color:red;
    }
 
|]




getCadastroR :: Handler Html
getCadastroR = do
             (widget, enctype) <- generateFormPost formPessoa
             defaultLayout $ widgetForm CadastroR enctype widget "Pessoas"

getPessoaR :: PessoaId -> Handler Html
getPessoaR pid = do
             pessoa <- runDB $ get404 pid 
             defaultLayout [whamlet| 
                 <h1> Seja bem-vindx #{pessoaNome pessoa}
                 <p> Salario: #{pessoaSalario pessoa}
                 <p> Idade: #{pessoaIdade pessoa}
             |]

getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc PessoaNome]
             defaultLayout [whamlet|
                 <h1> Pessoas cadastradas:
                 $forall Entity pid pessoa <- listaP
                     <a href=@{PessoaR pid}> #{pessoaNome pessoa} <br>
             |]

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       runDB $ insert pessoa 
                       defaultLayout [whamlet| 
                           <h1> #{pessoaNome pessoa} Inseridx com sucesso. 
                       |]
                    _ -> redirect CadastroR

getDeptoR :: Handler Html
getDeptoR = do
             (widget, enctype) <- generateFormPost formDepto
             defaultLayout $ widgetForm DeptoR enctype widget "Departamentos"

postDeptoR :: Handler Html
postDeptoR = do
                ((result, _), _) <- runFormPost formDepto
                case result of
                    FormSuccess depto -> do
                       runDB $ insert depto
                       defaultLayout [whamlet|
                           <h1> #{departamentoNome depto} Inserido com sucesso. 
                       |]
                    _ -> redirect DeptoR


getCampusR :: Handler Html
getCampusR = do
             (widget, enctype) <- generateFormPost formDepto
             defaultLayout $ widgetForm CampusR enctype widget "Campus"

postCampusR :: Handler Html
postCampusR = do
                ((result, _), _) <- runFormPost formDepto
                case result of
                    FormSuccess depto -> do
                       runDB $ insert depto
                       defaultLayout [whamlet|
                           <h1> Campus #{departamentoNome depto} Inserido com sucesso. 
                       |]
                    _ -> redirect DeptoR
                
formUsuario :: Form Usuario
formUsuario = renderDivs $ Usuario <$>
              areq textField "Login" Nothing <*>
              areq textField FieldSettings{fsId=Just "hident22", 
                           fsLabel="Password",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","8")]} Nothing --atributos a ser colocados
                           
getUsuarioR :: Handler Html
getUsuarioR = do
             (widget, enctype) <- generateFormPost formUsuario
             defaultLayout $ widgetForm UsuarioR enctype widget "Usuario"

postUsuarioR :: Handler Html
postUsuarioR = do
                ((result, _), _) <- runFormPost formUsuario
                case result of
                    FormSuccess usuario -> do
                       runDB $ insert usuario
                       defaultLayout [whamlet| 
                           <h1> Aluno #{usuarioLogin usuario} cadastrado com sucesso. 
                       |]
                    _ -> redirect UsuarioR

getProfessorR :: Handler Html
getProfessorR = do
             (widget, enctype) <- generateFormPost formUsuario
             defaultLayout $ widgetForm ProfessorR enctype widget "Professor"

postProfessorR :: Handler Html
postProfessorR = do
                ((result, _), _) <- runFormPost formUsuario
                case result of
                    FormSuccess usuario -> do
                       runDB $ insert usuario
                       defaultLayout [whamlet| 
                           <h1> Aluno #{usuarioLogin usuario} cadastrado com sucesso. 
                       |]
                    _ -> redirect UsuarioR




connStr = "dbname=d260oem2pj8u39 host=ec2-54-235-147-211.compute-1.amazonaws.com user=tnitzepbtajszh password=IfleJg3pyVcbxb1vdimnmyxljH port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warpEnv (Sitio pool)

