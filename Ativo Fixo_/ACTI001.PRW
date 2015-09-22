/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Programa  ³ ACTI001 ³ Autor ³  Márcio A. Zaguetti  ³ Data ³02/09/2013³  ±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Funções específicas TOTVS - Importações                    ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
#include "protheus.ch"
#include "rwmake.ch"
#include "topconn.ch"
#include "font.ch"
#include "msole.ch"
                      
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³xACTISF4  ºAutor  ³Márcio A. Zaguetti  º Data ³  02/10/13   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Importação de TES                                           º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Aceleradores TOTVS                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function xACTISF4(cSegm,cTab)
Local aMsExecAuto:={}
Local nAuxMod    :=nModulo
Local cFilAux    :=cFilAnt 
Local cPathArq   :="SF4"+AllTrim(cSegm)+".CSV"
Local lRet       :=.T.          
Local xaEmp      :=StrToKarr(u_xGetPar("CODEMP"),"/")    
Local nTotRec    :=0
Local nCount     :=0
Local cAux       :=Space(0)
Private lMsErroAuto
Private aArq     :={}
Private nErro    :=0
Private aCamp    :={}
Private cPath    := GetSrvProfString("Startpath","")
Private aErros   := {}                      

   // -> Fecha arquivo temporário
   Processa({|| aArq := AClone(u_xLeCSV(cPathArq,"CSV",";")) },"Agarde, Lendo Arquivo...")
   aCamp:=AClone(aArq[1,3])
   
   If Empty(aArq[1,3])
      If (Select(aArq[1,2]) <> 0)
	     dbSelectArea(aArq[1,2])
		 dbCloseArea()
	  Endif
	  MsgStop("Verifique o arquivo "+Alltrim(cPathArq),"Atenção")
	  lRet:=.F.
	  Return(.F.)
   Endif

   // -> Grava dados de Fornecedores
   &(aArq[1,2])->(dbGoTop())       
   nTotRec:=&(aArq[1,2])->(LastRec())
   ProcRegua(nTotRec)
   While &(aArq[1,2])->(!Eof()) 
      nCount++
      IncProc("Processando " + Str(nCount) + " de "+Str(nTotRec))
      If !Empty(AllTrim(&(aArq[1,2]+"->"+"F4_CODIGO")))
         nModulo    :=2  
         lMsErroAuto:=.F.                 
         aMsExecAuto:={}
         For z:=1 to Len(aCamp)
            If AllTrim(aCamp[z]) == "F4_FILIAL" 
               If !Empty(AllTrim(xFilial("SF4")))
                  cAux:=&(aArq[1,2]+"->F4_FILIAL")
                  // -> Verifica informação da filial do Arquivo
                  If Empty(AllTrim(cAux))
                     ShowHelpDlg("Erro", {"Tabela em modo exclusivo e filial do arquivo vazia!"},5,{"Favor verificar as informações do arquivo de importação ou alterar a tabela '"+cTab+"' para compartilgada."},5)
                     lRet:=.F.
                  EndIf
                  // -> Verifica se a filial passada no arquivo existe
                  If !Empty(AllTrim(cAux)) .and. !u_xPosFil(cempAnt,cAux)[1][2]
                     ShowHelpDlg("Erro", {"Filial inválida no arquivo de importação!"},5,{"Favor verificar as informações no campo '"+aCamp[z]+"' do arquivo."},5)
                     lRet:=.F.                     
                  EndIf
                  // -> Adiciona campo filial e seleciona filial corrente
                  If lRet
                     Aadd(aMsExecAuto, {aCamp[z],IIF(Empty(AllTrim(&(aArq[1,2]+"->F4_FILIAL"))), xFilial("SF4"), cAux), Nil} )
                     cFilAnt:=cAux
                  EndIf   
               EndIf
            Else
               Aadd(aMsExecAuto, {aCamp[z],&(aArq[1,2]+"->"+aCamp[z]), Nil} )
            EndIf   
         Next z         
         // -> Executa Processo
         If lRet
            MSExecAuto({|x,y| Mata080(x,y)},aMsExecAuto,3)        
            If lMsErroAuto
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->F4_CODIGO")+" - "+&(aArq[1,2]+"->F4_TEXTO")) 
               AADD(aErros,"============================================================================================================================" )
               cNomArqErro:= cTab+cSegm+"tmp.log"
               MostraErro(cPath, cNomArqErro )
               cMsgLog:=MemoRead(cPath+cNomArqErro)                                  
               AADD(aErros,MemoRead(cPath+cNomArqErro))
               MsErase(cNomArqErro)
               lRet := .F.
            Else
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->F4_CODIGO")+" - "+&(aArq[1,2]+"->F4_TEXTO"))
               AADD(aErros,"IMPORTAÇÃO OK -> CODIGO: "+&(aArq[1,2]+"->F4_CODIGO"))
               AADD(aErros,"============================================================================================================================" )
            EndIf
         EndIf      
         cFilAnt := cFilAux
         nModulo := nAuxMod
	  Endif
	  &(aArq[1,2])->(dbSkip())
   Enddo               
   &(aArq[1,2])->(dbCloseArea())
   // -> Verifica se Ocorreram erros
   If Len(aErros) > 0 .and. lRet
      // -> Grava Erros em arquivo de log
      u_xWhiteLog(aErros,"\import\"+cTab+cSegm)
      ShowHelpDlg("Erro", {"Ocorreram erros na execução da rotina!"},5,{"Favor verificar os erro no arquivo '"+cPath+cTab+cSegm+".log' gerado."},5)
   EndIf   
Return(lRet)


/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³xACTISA1  ºAutor  ³Márcio A. Zaguetti  º Data ³  02/10/13   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Importação SA1                                              º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Aceleradores TOTVS                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function xACTISA1(cSegm,cTab)
Local aMsExecAuto:={}
Local nAuxMod    :=nModulo
Local cFilAux    :=cFilAnt 
Local cPathArq   :=cTab+AllTrim(cSegm)+".CSV"
Local lRet       :=.T.          
Local lRetReg    :=.T.          
Local xaEmp      :=StrToKarr(u_xGetPar("CODEMP"),"/")    
Local nTotRec    :=0
Local nCount     :=0
Local cAux       :=Space(0)
Local cAuxMun    :=Space(0)
Local cCodi      :=Space(0)
Local cLoj       :=Space(0)
Local cFil       :=Space(0)
Private lMsErroAuto
Private aArq     :={}
Private nErro    :=0
Private aCamp    :={}              
Private cPath    := GetSrvProfString("Startpath","")
Private aErros   := {}                

   // -> Fecha arquivo temporário
   Processa({|| aArq := AClone(u_xLeCSV(cPathArq,"CSV",";")) },"Agarde, Lendo Arquivo...")
   aCamp:=AClone(aArq[1,3])
   
   If Empty(aArq[1,3])
      If (Select(aArq[1,2]) <> 0)
	     dbSelectArea(aArq[1,2])
		 dbCloseArea()
	  Endif
	  MsgStop("Verifique o arquivo "+Alltrim(cPathArq),"Atenção")
	  lRet:=.F.
	  Return(.F.)
   Endif

   // -> Grava dados de Fornecedores
   &(aArq[1,2])->(dbGoTop())       
   nTotRec:=&(aArq[1,2])->(LastRec())
   ProcRegua(nTotRec)
   While &(aArq[1,2])->(!Eof()) .and. lRet
      cCodi  :=Space(0)
      cLoj   :=Space(0)         
      cFil   :=Space(0)
      lRetReg:=.T.
      nCount++
      IncProc("Processando registro " + AllTrim(Str(nCount)) + " de "+AllTrim(Str(nTotRec)))
      If !Empty(AllTrim(&(aArq[1,2]+"->"+"A1_NOME")))
         nModulo    :=6  
         lMsErroAuto:=.F.                 
         aMsExecAuto:={}
         For z:=1 to Len(aCamp)
            // -> Tratamento de Filial
            If AllTrim(aCamp[z]) == "A1_FILIAL" 
               If !Empty(AllTrim(xFilial("SA1")))
                  cAux:=&(aArq[1,2]+"->A1_FILIAL")
                  cFil:=AllTrim(cAux)+Space(TamSx3("A1_FILIAL")[1]- Len(AllTrim(cAux)) )
                  // -> Verifica informação da filial do Arquivo
                  If Empty(AllTrim(cAux))
                     ShowHelpDlg("Erro", {"Tabela em modo exclusivo e filial do arquivo vazia!"},5,{"Favor verificar as informações do arquivo de importação ou alterar a tabela '"+cTab+"' para compartilgada."},5)
                     lRet:=.F.
                  EndIf
                  // -> Verifica se a filial passada no arquivo existe
                  If !Empty(AllTrim(cAux)) .and. !u_xPosFil(cempAnt,cAux)[1][2]
                     ShowHelpDlg("Erro", {"Filial inválida no arquivo de importação!"},5,{"Favor verificar as informações no campo '"+aCamp[z]+"' do arquivo."},5)
                     lRet:=.F.                     
                  EndIf
                  // -> Adiciona campo filial e seleciona filial corrente
                  If lRet
                     Aadd(aMsExecAuto, {aCamp[z],IIF(Empty(AllTrim(&(aArq[1,2]+"->A1_FILIAL"))), xFilial("SA1"), cAux), Nil} )
                     cFilAnt:=cAux
                  EndIf            
                  RecLock(aArq[1,2],.F.)         
                  &(aArq[1,2]+"->"+AllTrim(aCamp[z])):=cAux
                  MsUnlock(aArq[1,2])
               EndIf
            // -> Tratamento de CGC 
            ElseIf AllTrim(aCamp[z]) == "A1_CGC" .and. lRet
               cAux:=&(aArq[1,2]+"->A1_CGC")
               DbSelectArea("SA1")
               SA1->(DbSetOrder(3))
               If SA1->(DbSeek(xFilial("SA1")+cAux))
                  AADD(aErros,"INFO: --> "+cAux+" - "+&(aArq[1,2]+"->A1_NOME") )
                  AADD(aErros,"CNPJ OU CPF JA IMPORTADO - REGISTRO IGNORADO."  )
                  AADD(aErros,"============================================================================================================================" )
                  lRetReg:=.F.
               Else
                  Aadd(aMsExecAuto, {aCamp[z],cAux, Nil} )
               EndIf   
            // -> Tratamento de código
            ElseIf AllTrim(aCamp[z]) == "A1_COD" .and. lRet
               cAux:=u_xSeqCliI(&(aArq[1,2]+"->A1_CGC"),&(aArq[1,2]+"->A1_PESSOA"),"C")[1]
               Aadd(aMsExecAuto, {aCamp[z],cAux, Nil} )
               cCodi:=cAux
               RecLock(aArq[1,2],.F.)         
               &(aArq[1,2]+"->"+AllTrim(aCamp[z])):=cCodi
               MsUnlock(aArq[1,2])
            // -> Tratamento de Loja
            ElseIf AllTrim(aCamp[z]) == "A1_LOJA" .and. lRet
               cAux:=u_xSeqCliI(&(aArq[1,2]+"->A1_CGC"),&(aArq[1,2]+"->A1_PESSOA"),"C")[2]               
               Aadd(aMsExecAuto, {aCamp[z],cAux, Nil} )
               cLoj:=cAux
               RecLock(aArq[1,2],.F.)         
               &(aArq[1,2]+"->"+AllTrim(aCamp[z])):=cLoj
               MsUnlock(aArq[1,2])
            // -> Tratamento de códigos do IBGE
            ElseIf AllTrim(aCamp[z]) == "A1_COD_MUN" .and. lRet
               cAux:=&(aArq[1,2]+"->A1_EST")+IIF(Len(AllTrim(&(aArq[1,2]+"->A1_COD_MUN"))) > TamSx3("A1_COD_MUN")[1], SubStr(&(aArq[1,2]+"->A1_COD_MUN"),3,TamSx3("A1_COD_MUN")[1]),SubStr(&(aArq[1,2]+"->A1_COD_MUN"),1,TamSx3("A1_COD_MUN")[1]))
               DbSelectArea("CC2")
               CC2->(DbSetOrder(1))
               CC2->(DbSeek(xFilial("CC2")+cAux))
               cAuxMun:=CC2->CC2_MUN
               Aadd(aMsExecAuto, {aCamp[z],CC2->CC2_CODMUN, Nil} )
               RecLock(aArq[1,2],.F.)         
               &(aArq[1,2]+"->"+AllTrim(aCamp[z])):=CC2->CC2_CODMUN           
               MsUnlock(aArq[1,2])
            // -> Tratamento de códigos do IBGE
            ElseIf AllTrim(aCamp[z]) == "A1_INSCRM" .and. lRet
               cAux:=&(aArq[1,2]+"->A1_INSCRM")
               If !Empty(AllTrim(cAux))
                  Aadd(aMsExecAuto, {aCamp[z],cAux, Nil} )
               EndIf   
            ElseIf AllTrim(aCamp[z]) == "A1_MUN" .and. lRet
               Aadd(aMsExecAuto, {aCamp[z],cAuxMun, Nil} ) 
               RecLock(aArq[1,2],.F.)         
               &(aArq[1,2]+"->"+AllTrim(aCamp[z])):=cAuxMun           
               MsUnlock(aArq[1,2])
            ElseIf lRet
               Aadd(aMsExecAuto, {aCamp[z],&(aArq[1,2]+"->"+aCamp[z]), Nil} )
            EndIf   
         Next z         
         // -> Executa Processo
         If lRet .and. lRetReg
            MSExecAuto({|x,y| Mata030(x,y)},aMsExecAuto,3)        
            If lMsErroAuto                   
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->A1_CGC")+" - "+&(aArq[1,2]+"->A1_NOME") +"(codigo/loja:"+cCodi+"/"+cLoj+")" )
               AADD(aErros,"============================================================================================================================" )
               cNomArqErro:= cTab+cSegm+"tmp.log"
               MostraErro(cPath, cNomArqErro )
               cMsgLog:=MemoRead(cPath+cNomArqErro)                                  
               AADD(aErros,MemoRead(cPath+cNomArqErro))
               MsErase(cNomArqErro)    
            Else
               // -> Verifica se o registro existe, senão inclui
               DbSelectArea(cTab)
               DbSetOrder(1)
               If !DbSeek(cFil+cCodi+cLoj)
                  RecLock(cTab,.T.)         
                  For z:=1 to Len(aMsExecAuto)
                     &(cTab+"->"+aMsExecAuto[z,1]+":="+aArq[1,2]+"->"+aMsExecAuto[z,1])
                  Next z           
                  MsUnlock(cTab)
               EndIf
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->A1_CGC")+" - "+&(aArq[1,2]+"->A1_NOME") )
               AADD(aErros,"IMPORTAÇÃO OK -> COD/LOJA "+cCodi+"/"+cLoj )
               AADD(aErros,"============================================================================================================================" )
            EndIf
         EndIf      
         cFilAnt := cFilAux
         nModulo := nAuxMod
	  Endif
	  &(aArq[1,2])->(dbSkip())
   Enddo               
   &(aArq[1,2])->(dbCloseArea())
   // -> Verifica se Ocorreram erros
   If Len(aErros) > 0 .and. lRet
      // -> Grava Erros em arquivo de log
      u_xWhiteLog(aErros,"\import\"+cTab+cSegm)
      ShowHelpDlg("Erro", {"Ocorreram erros na execução da rotina!"},5,{"Favor verificar os erro no arquivo '"+cPath+cTab+cSegm+".log' gerado."},5)
   EndIf   
Return(lRet)


/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³xACTISA2  ºAutor  ³Márcio A. Zaguetti  º Data ³  02/10/13   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Importação SA2                                              º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Aceleradores TOTVS                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function xACTISA2(cSegm,cTab)
Local aMsExecAuto:={}
Local nAuxMod    :=nModulo
Local cFilAux    :=cFilAnt 
Local cPathArq   :=cTab+AllTrim(cSegm)+".CSV"
Local lRet       :=.T.          
Local lRetReg    :=.T.          
Local xaEmp      :=StrToKarr(u_xGetPar("CODEMP"),"/")    
Local nTotRec    :=0
Local nCount     :=0
Local cAux       :=Space(0)
Local cAuxMun    :=Space(0)
Local cCodi      :=Space(0)
Local cLoj       :=Space(0)
Private lMsErroAuto
Private aArq     :={}
Private nErro    :=0
Private aCamp    :={}              
Private cPath    := GetSrvProfString("Startpath","")
Private aErros   := {}                      
Public  lExecu   :=.T.

   // -> Fecha arquivo temporário
   Processa({|| aArq := AClone(u_xLeCSV(cPathArq,"CSV",";")) },"Agarde, Lendo Arquivo...")
   aCamp:=AClone(aArq[1,3])
   
   If Empty(aArq[1,3])
      If (Select(aArq[1,2]) <> 0)
	     dbSelectArea(aArq[1,2])
		 dbCloseArea()
	  Endif
	  MsgStop("Verifique o arquivo "+Alltrim(cPathArq),"Atenção")
	  lRet:=.F.
	  Return(.F.)
   Endif

   // -> Grava dados de Fornecedores
   &(aArq[1,2])->(dbGoTop())       
   nTotRec:=&(aArq[1,2])->(LastRec())
   ProcRegua(nTotRec)
   While &(aArq[1,2])->(!Eof()) .and. lRet
      cCodi  :=Space(0)
      cLoj   :=Space(0)         
      lRetReg:=.T.
      nCount++
      
      If nCount == 1737
         Alert("ok")
      EndIf
      
      IncProc("Processando registro " + AllTrim(Str(nCount)) + " de "+AllTrim(Str(nTotRec)))
      If !Empty(AllTrim(&(aArq[1,2]+"->"+"A2_NOME")))         
         nModulo    :=6  
         lMsErroAuto:=.F.                 
         aMsExecAuto:={}
         For z:=1 to Len(aCamp)
            // -> Tratamento de Filial
            If AllTrim(aCamp[z]) == "A2_FILIAL" 
               If !Empty(AllTrim(xFilial("SA2")))
                  cAux:=&(aArq[1,2]+"->A2_FILIAL")
                  // -> Verifica informação da filial do Arquivo
                  If Empty(AllTrim(cAux))
                     ShowHelpDlg("Erro", {"Tabela em modo exclusivo e filial do arquivo vazia!"},5,{"Favor verificar as informações do arquivo de importação ou alterar a tabela '"+cTab+"' para compartilgada."},5)
                     lRet:=.F.
                  EndIf
                  // -> Verifica se a filial passada no arquivo existe
                  If !Empty(AllTrim(cAux)) .and. !u_xPosFil(cempAnt,cAux)[1][2]
                     ShowHelpDlg("Erro", {"Filial inválida no arquivo de importação!"},5,{"Favor verificar as informações no campo '"+aCamp[z]+"' do arquivo."},5)
                     lRet:=.F.                     
                  EndIf
                  // -> Adiciona campo filial e seleciona filial corrente
                  If lRet
                     Aadd(aMsExecAuto, {aCamp[z],IIF(Empty(AllTrim(&(aArq[1,2]+"->A2_FILIAL"))), xFilial("SA2"), cAux), Nil} )
                     cFilAnt:=cAux
                  EndIf   
               EndIf
            // -> Tratamento de CGC 
            ElseIf AllTrim(aCamp[z]) == "A2_CGC" .and. lRet
               cAux:=&(aArq[1,2]+"->A2_CGC")
               DbSelectArea("SA2")
               SA2->(DbSetOrder(3))
               If SA2->(DbSeek(xFilial("SA2")+cAux))
                  AADD(aErros,"INFO: --> "+cAux+" - "+&(aArq[1,2]+"->A2_NOME") )
                  AADD(aErros,"CNPJ OU CPF JA IMPORTADO - REGISTRO IGNORADO."  )
                  AADD(aErros,"============================================================================================================================" )
                  lRetReg:=.F.
               Else
                  Aadd(aMsExecAuto, {aCamp[z],cAux, Nil} )
               EndIf   
            // -> Tratamento de código
            ElseIf AllTrim(aCamp[z]) == "A2_COD" .and. lRet
               cAux:=u_xSeqForI(&(aArq[1,2]+"->A2_CGC"),&(aArq[1,2]+"->A2_TIPO"),"C")[1]
               Aadd(aMsExecAuto, {aCamp[z],cAux, Nil} )
               cCodi:=cAux
            // -> Tratamento de Loja
            ElseIf AllTrim(aCamp[z]) == "A2_LOJA" .and. lRet
               cAux:=u_xSeqForI(&(aArq[1,2]+"->A2_CGC"),&(aArq[1,2]+"->A2_TIPO"),"C")[2]               
               Aadd(aMsExecAuto, {aCamp[z],cAux, Nil} )
               cLoj :=cAux
            // -> Tratamento de códigos do IBGE
            ElseIf AllTrim(aCamp[z]) == "A2_COD_MUN" .and. lRet
               cAux:=&(aArq[1,2]+"->A2_EST")+IIF(Len(AllTrim(&(aArq[1,2]+"->A2_COD_MUN"))) > TamSx3("A2_COD_MUN")[1], SubStr(&(aArq[1,2]+"->A2_COD_MUN"),3,TamSx3("A2_COD_MUN")[1]),SubStr(&(aArq[1,2]+"->A2_COD_MUN"),1,TamSx3("A2_COD_MUN")[1]))
               DbSelectArea("CC2")
               CC2->(DbSetOrder(1))
               CC2->(DbSeek(xFilial("CC2")+cAux))
               cAuxMun:=CC2->CC2_MUN
               Aadd(aMsExecAuto, {aCamp[z],CC2->CC2_CODMUN, Nil} )
            // -> Tratamento de códigos do IBGE
            ElseIf AllTrim(aCamp[z]) == "A2_INSCRM" .and. lRet
               cAux:=&(aArq[1,2]+"->A2_INSCRM")
               If !Empty(AllTrim(cAux))
                  Aadd(aMsExecAuto, {aCamp[z],cAux, Nil} )
               EndIf   
            ElseIf AllTrim(aCamp[z]) == "A2_MUN" .and. lRet
               Aadd(aMsExecAuto, {aCamp[z],cAuxMun, Nil} )            
            ElseIf lRet
               Aadd(aMsExecAuto, {aCamp[z],&(aArq[1,2]+"->"+aCamp[z]), Nil} )
            EndIf   
         Next z                                                                               
         
         // -> Executa Processo
         If lRet .and. lRetReg
            MSExecAuto({|x,y| Mata020(x,y)},aMsExecAuto,3)        
            If lMsErroAuto                             
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->A2_CGC")+" - "+&(aArq[1,2]+"->A2_NOME")+ "(codido/loja:"+cCodi+"/"+cLoj+")" )
               AADD(aErros,"============================================================================================================================" )
               cNomArqErro:= cTab+cSegm+"tmp.log"
               MostraErro(cPath, cNomArqErro )
               cMsgLog:=MemoRead(cPath+cNomArqErro)                                  
               AADD(aErros,MemoRead(cPath+cNomArqErro))
               MsErase(cNomArqErro)
            Else
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->A2_CGC")+" - "+&(aArq[1,2]+"->A2_NOME") )
               AADD(aErros,"IMPORTAÇÃO OK -> COD/LOJA "+cCodi+"/"+cLoj )
               AADD(aErros,"============================================================================================================================" )
            EndIf
         EndIf      
         cFilAnt := cFilAux
         nModulo := nAuxMod
	  Endif
	  &(aArq[1,2])->(dbSkip())
   Enddo               
   &(aArq[1,2])->(dbCloseArea())
   // -> Verifica se Ocorreram erros
   If Len(aErros) > 0 .and. lRet
      // -> Grava Erros em arquivo de log
      u_xWhiteLog(aErros,"\import\"+cTab+cSegm)
      ShowHelpDlg("Erro", {"Ocorreram erros na execução da rotina!"},5,{"Favor verificar os erro no arquivo '"+cPath+cTab+cSegm+".log' gerado."},5)
   EndIf   
Return(lRet) 


/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³xACTISA4  ºAutor  ³Márcio A. Zaguetti  º Data ³  02/10/13   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Importação SA4                                              º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Aceleradores TOTVS                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function xACTISA4(cSegm,cTab)
Local aMsExecAuto:={}
Local nAuxMod    :=nModulo
Local cFilAux    :=cFilAnt 
Local cPathArq   :=cTab+AllTrim(cSegm)+".CSV"
Local lRet       :=.T.          
Local lRetReg    :=.T.          
Local xaEmp      :=StrToKarr(u_xGetPar("CODEMP"),"/")    
Local nTotRec    :=0
Local nCount     :=0
Local cAux       :=Space(0)
Local cAuxMun    :=Space(0)
Local cCodi      :=Space(0)
Private lMsErroAuto
Private aArq     :={}
Private nErro    :=0
Private aCamp    :={}              
Private cPath    := GetSrvProfString("Startpath","")
Private aErros   := {}                      

   // -> Fecha arquivo temporário
   Processa({|| aArq := AClone(u_xLeCSV(cPathArq,"CSV",";")) },"Agarde, Lendo Arquivo...")
   aCamp:=AClone(aArq[1,3])
   
   If Empty(aArq[1,3])
      If (Select(aArq[1,2]) <> 0)
	     dbSelectArea(aArq[1,2])
		 dbCloseArea()
	  Endif
	  MsgStop("Verifique o arquivo "+Alltrim(cPathArq),"Atenção")
	  lRet:=.F.
	  Return(.F.)
   Endif

   // -> Grava dados de Transportadoras
   &(aArq[1,2])->(dbGoTop())       
   nTotRec:=&(aArq[1,2])->(LastRec())
   ProcRegua(nTotRec)
   While &(aArq[1,2])->(!Eof()) .and. lRet      
      
      cCodi  :=Space(0)                           
      cAuxMun:=space(0)
      
      lRetReg:=.T.
      nCount++
            
      IncProc("Processando registro " + AllTrim(Str(nCount)) + " de "+AllTrim(Str(nTotRec)))
      If !Empty(AllTrim(&(aArq[1,2]+"->"+"A4_NOME")))         
         nModulo    :=5  
         lMsErroAuto:=.F.                 
         aMsExecAuto:={}
         For z:=1 to Len(aCamp)
            // -> Tratamento de Filial
            If AllTrim(aCamp[z]) == "A4_FILIAL" 
               If !Empty(AllTrim(xFilial("SA4")))
                  cAux:=&(aArq[1,2]+"->A4_FILIAL")
                  // -> Verifica informação da filial do Arquivo
                  If Empty(AllTrim(cAux))
                     ShowHelpDlg("Erro", {"Tabela em modo exclusivo e filial do arquivo vazia!"},5,{"Favor verificar as informações do arquivo de importação ou alterar a tabela '"+cTab+"' para compartilgada."},5)
                     lRet:=.F.
                  EndIf
                  // -> Verifica se a filial passada no arquivo existe
                  //If !Empty(AllTrim(cAux)) .and. !u_xPosFil(cempAnt,cAux)[1][2]
                  //   ShowHelpDlg("Erro", {"Filial inválida no arquivo de importação!"},5,{"Favor verificar as informações no campo '"+aCamp[z]+"' do arquivo."},5)
                  //   lRet:=.F.                     
                  //EndIf
                  // -> Adiciona campo filial e seleciona filial corrente
                  If lRet
                     Aadd(aMsExecAuto, {aCamp[z],IIF(Empty(AllTrim(&(aArq[1,2]+"->A4_FILIAL"))), xFilial("SA4"), cAux), Nil} )
                     cFilAnt:=cAux
                  EndIf   
               EndIf
            // -> Tratamento de CGC 
            ElseIf AllTrim(aCamp[z]) == "A4_CGC" .and. lRet
               cAux:=&(aArq[1,2]+"->A4_CGC")
               DbSelectArea("SA4")
               SA4->(DbSetOrder(3))
               If SA4->(DbSeek(xFilial("SA4")+cAux))
                  AADD(aErros,"INFO: --> "+cAux+" - "+&(aArq[1,2]+"->A4_NOME") )
                  AADD(aErros,"CNPJ OU CPF JA IMPORTADO - REGISTRO IGNORADO."  )
                  AADD(aErros,"============================================================================================================================" )
                  lRetReg:=.F.
               Else
                  Aadd(aMsExecAuto, {aCamp[z],cAux, Nil} )
               EndIf   
            // -> Tratamento de código
            ElseIf AllTrim(aCamp[z]) == "A4_COD" .and. lRet
               cAux:=u_xSeqTran()
               Aadd(aMsExecAuto, {aCamp[z],cAux, Nil} )
               cCodi:=cAux
            ElseIf AllTrim(aCamp[z]) == "A4_COD_MUN" .and. lRet
               cAux:=&(aArq[1,2]+"->A4_EST")+IIF(Len(AllTrim(&(aArq[1,2]+"->A4_COD_MUN"))) > TamSx3("A4_COD_MUN")[1], SubStr(&(aArq[1,2]+"->A4_COD_MUN"),3,TamSx3("A4_COD_MUN")[1]),SubStr(&(aArq[1,2]+"->A4_COD_MUN"),1,TamSx3("A4_COD_MUN")[1]))
               DbSelectArea("CC2")
               CC2->(DbSetOrder(1))
               CC2->(DbSeek(xFilial("CC2")+cAux))
               cAuxMun:=CC2->CC2_MUN
               Aadd(aMsExecAuto, {aCamp[z],CC2->CC2_CODMUN, Nil} )
            ElseIf AllTrim(aCamp[z]) == "A4_MUN" .and. lRet
               Aadd(aMsExecAuto, {aCamp[z],cAuxMun, Nil} )            
            ElseIf lRet
               Aadd(aMsExecAuto, {aCamp[z],&(aArq[1,2]+"->"+aCamp[z]), Nil} )
            EndIf   
         Next z                                                                               
         
         // -> Executa Processo
         If lRet .and. lRetReg
            MSExecAuto({|x,y| Mata050(x,y)},aMsExecAuto,3)        
            If lMsErroAuto                             
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->A4_CGC")+" - "+&(aArq[1,2]+"->A4_NOME")+ "(codido:"+cCodi+")" )
               AADD(aErros,"============================================================================================================================" )
               cNomArqErro:= cTab+cSegm+"tmp.log"
               MostraErro(cPath, cNomArqErro )
               cMsgLog:=MemoRead(cPath+cNomArqErro)                                  
               AADD(aErros,MemoRead(cPath+cNomArqErro))
               MsErase(cNomArqErro)
            Else
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->A4_CGC")+" - "+&(aArq[1,2]+"->A4_NOME") )
               AADD(aErros,"IMPORTAÇÃO OK -> CODIGO "+cCodi )
               AADD(aErros,"============================================================================================================================" )
            EndIf
         EndIf      
         cFilAnt := cFilAux
         nModulo := nAuxMod
	  Endif
	  &(aArq[1,2])->(dbSkip())
   Enddo               
   &(aArq[1,2])->(dbCloseArea())
   // -> Verifica se Ocorreram erros
   If Len(aErros) > 0 .and. lRet
      // -> Grava Erros em arquivo de log
      u_xWhiteLog(aErros,"\import\"+cTab+cSegm)
      ShowHelpDlg("Erro", {"Ocorreram erros na execução da rotina!"},5,{"Favor verificar os erro no arquivo '"+cPath+cTab+cSegm+".log' gerado."},5)
   EndIf   
Return(lRet) 




/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³xACTISD1  ºAutor  ³Márcio A. Zaguetti  º Data ³  02/10/13   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Importação SD1                                              º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Aceleradores TOTVS                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function xACTISB1(cSegm,cTab)
Local aMsExecAuto:={}
Local nAuxMod    :=nModulo
Local cFilAux    :=cFilAnt 
Local cPathArq   :=cTab+AllTrim(cSegm)+".CSV"
Local lRet       :=.T.          
Local xaTPar     :={}
Local nTotRec    :=0
Local nCount     :=0
Local cAux       :=Space(0)
Local cAuxMun    :=Space(0)
Local aAux       :={}
Local cCodP      :=Space(0)
Private lMsErroAuto
Private aArq     :={}
Private nErro    :=0
Private aCamp    :={}              
Private cPath    := GetSrvProfString("Startpath","")
Private aErros   := {}                      

   // -> Pega parâmetros
   Aadd(xaTPar,{"B1_GRUPO"  ,0,"",""})
   Aadd(xaTPar,{"B1_XFAMILI",0,"",""})
   Aadd(xaTPar,{"B1_XMEDIDA",0,"",""})
   Aadd(xaTPar,{"B1_COD"    ,0,"",""})
   For x:=1 to Len(xaTPar)
       aAux       :=StrToKarr(u_xGetPar(xaTPar[x,1]),"/")                         
       xaTPar[x,2]:=Val(AllTrim(aAux[1]))
       xaTPar[x,3]:=AllTrim(aAux[2])     
   Next x

   Processa({|| aArq := AClone(u_xLeCSV(cPathArq,"CSV",";")) },"Agarde, Lendo Arquivo...")
   aCamp:=AClone(aArq[1,3])
   
   // -> Fecha arquivo temporário
   If Empty(aArq[1,3])
      If (Select(aArq[1,2]) <> 0)
	     dbSelectArea(aArq[1,2])
		 dbCloseArea()
	  Endif
	  MsgStop("Verifique o arquivo "+Alltrim(cPathArq),"Atenção")
	  lRet:=.F.
	  Return(.F.)
   Endif

   // -> Grava dados de Fornecedores
   &(aArq[1,2])->(dbGoTop())       
   nTotRec:=&(aArq[1,2])->(LastRec())
   ProcRegua(nTotRec)
   While &(aArq[1,2])->(!Eof()) .and. lRet
      cCodP:=Space(0)
      nCount++
      IncProc("Processando registro " + AllTrim(Str(nCount)) + " de "+AllTrim(Str(nTotRec)))
      If !Empty(AllTrim(&(aArq[1,2]+"->"+"B1_DESC")))
         nModulo    :=6  
         lMsErroAuto:=.F.                 
         aMsExecAuto:={}
         For z:=1 to Len(aCamp)
            // -> Tratamento de Filial
            If AllTrim(aCamp[z]) == "B1_FILIAL" 
               If !Empty(AllTrim(xFilial("SB1")))
                  cAux:=&(aArq[1,2]+"->B1_FILIAL")
                  // -> Verifica informação da filial do Arquivo
                  If Empty(AllTrim(cAux))
                     ShowHelpDlg("Erro", {"Tabela em modo exclusivo e filial do arquivo vazia!"},5,{"Favor verificar as informações do arquivo de importação ou alterar a tabela '"+cTab+"' para compartilgada."},5)
                     lRet:=.F.
                  EndIf
                  // -> Verifica se a filial passada no arquivo existe
                  If !Empty(AllTrim(cAux)) .and. !u_xPosFil(cempAnt,cAux)[1][2]
                     ShowHelpDlg("Erro", {"Filial inválida no arquivo de importação!"},5,{"Favor verificar as informações no campo '"+aCamp[z]+"' do arquivo."},5)
                     lRet:=.F.                     
                  EndIf
                  // -> Adiciona campo filial e seleciona filial corrente
                  If lRet
                     Aadd(aMsExecAuto, {aCamp[z],IIF(Empty(AllTrim(&(aArq[1,2]+"->B1_FILIAL"))), xFilial("SB1"), cAux), Nil} )
                     cFilAnt:=cAux
                  EndIf   
               EndIf
            // -> Tratamento de código
            ElseIf AllTrim(aCamp[z]) == "B1_COD" .and. lRet
               // -> Tratamento do grupo
               cAux:=AllTrim((aArq[1,2]+"->B1_GRUPO"))
               If xaTPar[1,3] == "N"
	              nAux       :=Len(AllTrim(Str(Val(cAux))))                            
	              cAux       :=AllTrim(Str(Val(cAux)))
	              xaTPar[1,4]:=Replicate('0',xaTPar[1,2] - nAux ) + cAux
	           Else
	              xaTPar[1,4]:=cAux 
	           EndIf   
               // -> Tratamento da Familia
               cAux:=AllTrim(&(aArq[1,2]+"->B1_XFAMILI"))
               If xaTPar[2,3] == "N"
	              nAux       :=Len(AllTrim(Str(Val(cAux))))                            
	              cAux       :=AllTrim(Str(Val(cAux)))
	              xaTPar[2,4]:=Replicate('0',xaTPar[2,2] - nAux ) + cAux
	           Else
	              xaTPar[2,4]:=cAux 
	           EndIf   
               // -> Tratamento da Familia
               cAux:=AllTrim(&(aArq[1,2]+"->B1_XMEDIDA"))
               If xaTPar[3,3] == "N"
	              nAux       :=Len(AllTrim(Str(Val(cAux))))                            
	              cAux       :=AllTrim(Str(Val(cAux)))
	              xaTPar[3,4]:=Replicate('0',xaTPar[3,2] - nAux ) + cAux
	           Else
	              xaTPar[3,4]:=cAux 
	           EndIf   
               // -> Tratamento do sequencial
               cAux:=AllTrim(&(aArq[1,2]+"->B1_COD"))
               If xaTPar[4,3] == "N"
	              nAux       :=Len(AllTrim(Str(Val(cAux))))                            
	              cAux       :=AllTrim(Str(Val(cAux)))
	              xaTPar[4,4]:=Replicate('0',xaTPar[4,2] - nAux ) + cAux
	           Else
	              xaTPar[4,4]:=cAux 
	           EndIf   
               cAux:=u_xSeqPInd(&(aArq[1,2]+"->B1_GRUPO"),xaTPar[1,2],xaTPar[1,3],&(aArq[1,2]+"->B1_XFAMILI"),xaTPar[2,2],xaTPar[2,3],&(aArq[1,2]+"->B1_XMEDIDA"),xaTPar[3,2],xaTPar[3,3],xaTPar[4,2])
               Aadd(aMsExecAuto, {aCamp[z], cAux, Nil} )
               cCodP:=cAux
            ElseIf lRet
               Aadd(aMsExecAuto, {aCamp[z],&(aArq[1,2]+"->"+aCamp[z]), Nil} )
            EndIf   
         Next z                                                                               
         // -> Executa Processo
         If lRet
            MSExecAuto({|x,y| Mata010(x,y)},aMsExecAuto,3)        
            If lMsErroAuto                             
               AADD(aErros,"INFO: --> "+cCodP+" - "+&(aArq[1,2]+"->B1_DESC") )
               AADD(aErros,"============================================================================================================================" )
               cNomArqErro:= cTab+cSegm+"tmp.log"
               MostraErro(cPath, cNomArqErro )
               cMsgLog:=MemoRead(cPath+cNomArqErro)                                  
               AADD(aErros,MemoRead(cPath+cNomArqErro))
               MsErase(cNomArqErro)
            Else
               AADD(aErros,"INFO: --> "+cCodP+" - "+&(aArq[1,2]+"->B1_DESC") )
               AADD(aErros,"IMPORTAÇÃO OK -> CODIGO: "+cCodP )
               AADD(aErros,"============================================================================================================================" )
            EndIf
         EndIf      
         cFilAnt := cFilAux
         nModulo := nAuxMod
	  Endif
	  &(aArq[1,2])->(dbSkip())
   Enddo               
   &(aArq[1,2])->(dbCloseArea())
   // -> Verifica se Ocorreram erros
   If Len(aErros) > 0 .and. lRet
      // -> Grava Erros em arquivo de log
      u_xWhiteLog(aErros,"\import\"+cTab+cSegm)
      ShowHelpDlg("Erro", {"Ocorreram erros na execução da rotina!"},5,{"Favor verificar os erro no arquivo '"+cPath+cTab+cSegm+".log' gerado."},5)
   EndIf   
Return(lRet)



/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³xACTISRA  ºAutor  ³Márcio A. Zaguetti  º Data ³  02/10/13   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Importação SRA                                              º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Aceleradores TOTVS                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function xACTISRA(cSegm,cTab)
Local aMsExecAuto:={}
Local nAuxMod    :=nModulo
Local cFilAux    :=cFilAnt 
Local cPathArq   :=cTab+AllTrim(cSegm)+".CSV"
Local lRet       :=.T.          
Local nTotRec    :=0
Local nCount     :=0
Local cAux       :=Space(0)
Local cAuxMun    :=Space(0)
Local aAux       :={}              
Local lFieldCat  :=.F.
Private lMsErroAuto
Private aArq     :={}
Private nErro    :=0
Private aCamp    :={}              
Private cPath    := GetSrvProfString("Startpath","")
Private aErros   := {}                      

   Processa({|| aArq := AClone(u_xLeCSV(cPathArq,"CSV",";")) },"Agarde, Lendo Arquivo...")
   aCamp:=AClone(aArq[1,3])
   
   // -> Fecha arquivo temporário
   If Empty(aArq[1,3])
      If (Select(aArq[1,2]) <> 0)
	     dbSelectArea(aArq[1,2])
		 dbCloseArea()
	  Endif
	  MsgStop("Verifique o arquivo "+Alltrim(cPathArq),"Atenção")
	  lRet:=.F.
	  Return(.F.)
   Endif

   // -> Verifica se existe campo de Autonomos
   For z:=1 to Len(aCamp)
      If AllTrim(aCamp[z]) == "RA_CATFUNC"
         lFieldCat:=.T.
      EndIf
   Next z
   
   // -> Grava dados de funcionários / Autonomos
   &(aArq[1,2])->(dbGoTop())       
   nTotRec:=&(aArq[1,2])->(LastRec())
   ProcRegua(nTotRec)
   While &(aArq[1,2])->(!Eof()) .and. lRet
      nCount++
      IncProc("Processando registro " + AllTrim(Str(nCount)) + " de "+AllTrim(Str(nTotRec)))
      If !Empty(AllTrim(&(aArq[1,2]+"->"+"RA_NOME")))
         nModulo    :=6  
         lMsErroAuto:=.F.                 
         aMsExecAuto:={}
         
         // -> Categoria do funcionário
         If lFieldCat
            cCateg:=AllTrim((aArq[1,2]+"->RA_CATFUNC"))
         Else 
            cCateg:="M"
         EndIf
         
         // -> Validação de campos específicos
         For z:=1 to Len(aCamp)
            // -> Tratamento de Filial
            If AllTrim(aCamp[z]) == "RA_FILIAL" 
               If !Empty(AllTrim(xFilial("SRA")))
                  cAux:=&(aArq[1,2]+"->RA_FILIAL")
                  // -> Verifica informação da filial do Arquivo
                  If Empty(AllTrim(cAux))
                     ShowHelpDlg("Erro", {"Tabela em modo exclusivo e filial do arquivo vazia!"},5,{"Favor verificar as informações do arquivo de importação ou alterar a tabela '"+cTab+"' para compartilgada."},5)
                     lRet:=.F.
                  EndIf
                  // -> Verifica se a filial passada no arquivo existe
                  If !Empty(AllTrim(cAux)) .and. !u_xPosFil(cempAnt,cAux)[1][2]
                     ShowHelpDlg("Erro", {"Filial inválida no arquivo de importação!"},5,{"Favor verificar as informações no campo '"+aCamp[z]+"' do arquivo."},5)
                     lRet:=.F.                     
                  EndIf
                  // -> Adiciona campo filial e seleciona filial corrente
                  If lRet
                     Aadd(aMsExecAuto, {aCamp[z],IIF(Empty(AllTrim(&(aArq[1,2]+"->RA_FILIAL"))), xFilial("SRA"), cAux), Nil} )
                     cFilAnt:=cAux
                  EndIf   
               EndIf                
            // -> Tratamento de Situação da folha
            ElseIf AllTrim(aCamp[z]) == "RA_SITFOLH" 
               cAux:=AllTrim(DtoS(&(aArq[1,2]+"->RA_DEMISSA")))
               If !Empty(cAux)
                  Aadd(aMsExecAuto, {aCamp[z], "D", Nil} )
               EndIf
            ElseIf lRet
               Aadd(aMsExecAuto, {aCamp[z],&(aArq[1,2]+"->"+aCamp[z]), Nil} )
            EndIf   
         Next z                                                                               
         // -> Executa Processo
         If lRet
            MSExecAuto({|x,y| Mata010(x,y)},aMsExecAuto,3)        
            If lMsErroAuto                             
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->RA_MAT")+" - "+&(aArq[1,2]+"->RA_NOME") )
               AADD(aErros,"============================================================================================================================" )
               cNomArqErro:= cTab+cSegm+"tmp.log"
               MostraErro(cPath, cNomArqErro )
               cMsgLog:=MemoRead(cPath+cNomArqErro)                                  
               AADD(aErros,MemoRead(cPath+cNomArqErro))
               MsErase(cNomArqErro)
            EndIf
         EndIf      
         cFilAnt := cFilAux
         nModulo := nAuxMod
	  Endif
	  &(aArq[1,2])->(dbSkip())
   Enddo               
   &(aArq[1,2])->(dbCloseArea())
   // -> Verifica se Ocorreram erros
   If Len(aErros) > 0 .and. lRet
      // -> Grava Erros em arquivo de log
      u_xWhiteLog(aErros,"\import\"+cTab+cSegm)
      ShowHelpDlg("Erro", {"Ocorreram erros na execução da rotina!"},5,{"Favor verificar os erro no arquivo '"+cPath+cTab+cSegm+".log' gerado."},5)
   EndIf   
Return(lRet)

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³xACTISN1  ºAutor  ³Márcio A. Zaguetti  º Data ³  02/10/13   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Importação SN1 / SN3                                        º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Aceleradores TOTVS                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function xACTISN1(cSegm,cTab)
Local aItens     :={}
Local aAux       :={}
Local aCab       :={}
Local aDadosAuto :={} 
Local nAuxMod    :=nModulo
Local cFilAux    :=cFilAnt 
//Local cPathArq   :=cTab+AllTrim(cSegm)+".CSV"
Local cPathArq   :="C:\alc\SN2010101.CSV"
Local lRet       :=.T.          
Local nTotRec    :=0
Local nCount     :=0
Local cAux       :=Space(0)
Local cAuxMun    :=Space(0)
Local lField     :=.F.
Private aArq     :={}
Private nErro    :=0
Private aCamp    :={}              
Private cPath    := GetSrvProfString("Startpath","")
Private aErros   := {}                      

   Processa({|| aArq := AClone(u_xLeCSV(cPathArq,"CSV",";",.T.)) },"Agarde, Lendo Arquivo...")
   aCamp:=AClone(aArq[1,3])
   
   // -> Fecha arquivo temporário
   If Empty(aArq[1,3])
      If (Select(aArq[1,2]) <> 0)
	     dbSelectArea(aArq[1,2])
		 dbCloseArea()
	  Endif
	  MsgStop("Verifique o arquivo "+Alltrim(cPathArq),"Atenção")
	  lRet:=.F.
	  Return(.F.)
   Endif
   
   // -> Grava dados de Ativos
   &(aArq[1,2])->(dbGoTop())       
   nTotRec:=&(aArq[1,2])->(LastRec())
   ProcRegua(nTotRec)
   While &(aArq[1,2])->(!Eof()) .and. lRet
      nCount++
      IncProc("Processando registro " + AllTrim(Str(nCount)) + " de "+AllTrim(Str(nTotRec)))
      If !Empty(AllTrim(&(aArq[1,2]+"->"+"N1_CBASE")))
         nModulo    :=1
         lMsErroAuto:=.F.                 
         aCab:={}
         aAux:={}
         //aadd(aItens,{})

         // -> Verifica se existe campo Filial
         lField:=.F.
         For b:=1 to Len(aCamp)
            If AllTrim(aCamp[b]) == "N1_FILIAL"
               lField:=.T.
            EndIf
         Next b     
         // -> Adiciona Campo  
         If !lField
            Aadd(aCab, {"N1_FILIAL",xFilial("SN1"), Nil} )
         EndIf

         // -> Verifica se existe campo Tipo
         lField:=.F.
         For b:=1 to Len(aCamp)
             If AllTrim(aCamp[b]) == "N3_TIPO"
                lField:=.T.
             EndIf
         Next b     
         // -> Adiciona Campo  
         If !lField
            Aadd(aaux, {"N3_TIPO","01", Nil} )
         EndIf

         // -> Verifica se existe campo de Histórico
         lField:=.F.
         For b:=1 to Len(aCamp)
             If AllTrim(aCamp[b]) == "N3_HISTOR"
                lField:=.T.
             EndIf
         Next b     
         // -> Adiciona Campo  
         If !lField
            Aadd(aaux, {"N3_HISTOR","SALDO INICIAL", Nil} )
         EndIf
                                                            

         // -> Verifica se existe campo de Tipo de Saldo
         lField:=.F.
         For b:=1 to Len(aCamp)
            If AllTrim(aCamp[b]) == "N3_TPSALDO"
               lField:=.T.
            EndIf
         Next b     
         // -> Adiciona Campo  
         If !lField
            Aadd(aaux, {"N3_TPSALDO","1", Nil} )
         EndIf
   
         // -> Verifica se existe campo de Tipo de Depreciação
         lField:=.F.
         For b:=1 to Len(aCamp)
             If AllTrim(aCamp[b]) == "N3_TPDEPR"
                lField:=.T.
             EndIf
         Next b     
         // -> Adiciona Campo  
         If !lField
            Aadd(aaux, {"N3_TPDEPR","1", Nil} )
         EndIf

         // -> Verifica se existe campo de Data de Aquisicao
         lField:=.F.
         For b:=1 to Len(aCamp)
             If AllTrim(aCamp[b]) == "N1_AQUISIC"
                lField:=.T.
             EndIf
         Next b     
         // -> Adiciona Campo  
         If !lField
            Aadd(aaux, {"N1_AQUISIC","1", Nil} )
         EndIf


         // -> Validação de campos específicos
         For q:=1 to Len(aCamp)
                     
            // - > Monta Array do Cabeçalho
            If SubStr(aCamp[q],1,2) == "N1" 
               // -> Tratamento de Filial
               If AllTrim(aCamp[q]) == "N1_FILIAL" 
                  If !Empty(AllTrim(xFilial("SN1")))
                     cAux:=&(aArq[1,2]+"->N1_FILIAL")
                     // -> Verifica informação da filial do Arquivo
                     If Empty(AllTrim(cAux))
                        ShowHelpDlg("Erro", {"Tabela em modo exclusivo e filial do arquivo vazia!"},5,{"Favor verificar as informações do arquivo de importação ou alterar a tabela '"+cTab+"' para compartilgada."},5)
                        lRet:=.F.
                     EndIf
                     // -> Verifica se a filial passada no arquivo existe
                     If Empty(AllTrim(cAux)) //!Empty(AllTrim(cAux)) //.and. !u_xPosFil(cempAnt,cAux)//[1][3]
                        ShowHelpDlg("Erro", {"Filial inválida no arquivo de importação!"},5,{"Favor verificar as informações no campo '"+aCamp[q]+"' do arquivo."},5)
                        lRet:=.F.                     
                     EndIf
                     // -> Adiciona campo filial e seleciona filial corrente
                     If lRet
                        Aadd(acab, {aCamp[q],IIF(Empty(AllTrim(&(aArq[1,2]+"->N1_FILIAL"))), xFilial("SN1"), cAux), Nil} )
                        cFilAnt:=cAux             
                     EndIf   
                  EndIf                
               // -> Tratamento da data de aquisição
               ElseIf AllTrim(aCamp[q]) == "N1_AQUISIC" 
                  cAux:=&(aArq[1,2]+"->N1_AQUISIC")
                  Aadd(acab, {aCamp[q],cAux, Nil} )
               ElseIf lRet
                  Aadd(aCab,{aCamp[q],&(aArq[1,2]+"->"+aCamp[q]), Nil} )
               EndIf   
            ElseIf SubStr(aCamp[q],1,2) == "N3" .and. lRet
               Aadd(aaux, {aCamp[q],&(aArq[1,2]+"->"+aCamp[q]), Nil} )
            EndIf
         Next q                                                                               
    
         // -> Executa Processo
         aItens:={}
         Aadd(aItens,{})      
         aItens[1]:=aClone(aAux)
         If lRet
            MSExecAuto( {|X,Y,Z| ATFA010(X,Y,Z)},aCab ,aItens, 3)    
//            MSExecAuto( {|X,Y,Z,W| ATFA010(X,Y,Z,W)} ,aCab ,aItens, 3)
            If lMsErroAuto                             
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->N1_CBASE")+" - "+&(aArq[1,2]+"->N1_DESCRIC") )
               AADD(aErros,"============================================================================================================================" )
//               cNomArqErro:= cTab+cSegm+"tmp.log"
               cNomArqErro:= "SN2tmp.log"
               MostraErro(cPath, cNomArqErro )
               cMsgLog:=MemoRead(cPath+cNomArqErro)                                  
               AADD(aErros,MemoRead(cPath+cNomArqErro))
               MsErase(cNomArqErro)
            Else
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->N1_CBASE")+" - "+&(aArq[1,2]+"->N1_DESCRIC") )
               AADD(aErros,"IMPORTAÇÃO OK " )
               AADD(aErros,"============================================================================================================================" )
            EndIf
         EndIf      
         cFilAnt := cFilAux
         nModulo := nAuxMod
	  Endif
	  &(aArq[1,2])->(dbSkip())
   Enddo               
   &(aArq[1,2])->(dbCloseArea())
   // -> Verifica se Ocorreram erros
   If Len(aErros) > 0 .and. lRet
      // -> Grava Erros em arquivo de log
      //u_xWhiteLog(aErros,"\import\"+cTab+cSegm)
      u_xWhiteLog(aErros,"\import\SN2tmp")
      ShowHelpDlg("Erro", {"Ocorreram erros na execução da rotina!"},5,{"Favor verificar os erro no arquivo '"+"\import\SN2tmp"+".log' gerado."},5)
   EndIf   
Return(lRet)


/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³xACTICT5  ºAutor  ³Márcio A. Zaguetti  º Data ³  02/10/13   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Importação CT5                                              º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Aceleradores TOTVS                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function xACTICT5(cSegm,cTab)
Local cPathArq   :=cTab+AllTrim(cSegm)+".CSV"
Local lRet       :=.T.          
Local nTotRec    :=0
Local nCount     :=0
Local cAux       :=Space(0)
Private lMsErroAuto
Private aArq     :={}
Private aCamp    :={}              
Private cPath    := GetSrvProfString("Startpath","")
Private aErros   := {}                      

   Processa({|| aArq := AClone(u_xLeCSV(cPathArq,"CSV",";")) },"Agarde, Lendo Arquivo...")
   aCamp:=AClone(aArq[1,3])
   
   // -> Fecha arquivo temporário
   If Empty(aArq[1,3])
      If (Select(aArq[1,2]) <> 0)
	     dbSelectArea(aArq[1,2])
		 dbCloseArea()
	  Endif
	  MsgStop("Verifique o arquivo "+Alltrim(cPathArq),"Atenção")
	  lRet:=.F.
	  Return(.F.)
   Endif
   
   // -> Grava dados de funcionários / Autonomos
   &(aArq[1,2])->(dbGoTop())       
   nTotRec:=&(aArq[1,2])->(LastRec())
   ProcRegua(nTotRec)
   While &(aArq[1,2])->(!Eof()) .and. lRet
      lRet:=.T.
      nCount++
      IncProc("Processando registro " + AllTrim(Str(nCount)) + " de "+AllTrim(Str(nTotRec)))
      If !Empty(AllTrim(&(aArq[1,2]+"->"+"CT5_LANPAD")))
         DbSelectArea(cTab)
         If RecLock(cTab,.T.)         
            // -> Validação de campos específicos
            For z:=1 to Len(aCamp)
               // -> Tratamento de Filial
               If AllTrim(aCamp[z]) == "CT5_FILIAL" 
                  If !Empty(AllTrim(xFilial("CT5")))
                     cAux:=aArq[1,2]+"->CT5_FILIAL"
                     // -> Verifica informação da filial do Arquivo
                     If Empty(AllTrim(cAux))
                        ShowHelpDlg("Erro", {"Tabela em modo exclusivo e filial do arquivo vazia!"},5,{"Favor verificar as informações do arquivo de importação ou alterar a tabela '"+cTab+"' para compartilgada."},5)
                        lRet:=.F.
                     EndIf
                     // -> Verifica se a filial passada no arquivo existe
                     If !Empty(AllTrim(cAux)) .and. !u_xPosFil(cempAnt,cAux)[1][2]
                        ShowHelpDlg("Erro", {"Filial inválida no arquivo de importação!"},5,{"Favor verificar as informações no campo '"+aCamp[z]+"' do arquivo."},5)
                        lRet:=.F.                        
                     EndIf
                     // -> Adiciona campo filial e seleciona filial corrente
                     If lRet
                        &(cTab+"->"+aCamp[z]+":="+aArq[1,2]+"->CT5_FILIAL")
                     EndIf   
                  EndIf                
               ElseIf lRet
                  &(cTab+"->"+aCamp[z]+":="+aArq[1,2]+"->"+aCamp[z])
               EndIf                                                                              
            Next z                                                                                
            // -> Gera Log
            If lRet
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->CT5_LANPAD")+" - "+&(aArq[1,2]+"->CT5_DESC") )
               AADD(aErros,"============================================================================================================================" )
               cMsgLog:="Importação OK"
               AADD(aErros,cMsgLog)
            Else
               AADD(aErros,"INFO: --> "+&(aArq[1,2]+"->CT5_LANPAD")+" - "+&(aArq[1,2]+"->CT5_DESC") )
               AADD(aErros,"============================================================================================================================" )
               cMsgLog:="Importação OK"
               AADD(aErros,cMsgLog)
            EndIf      
            MsUnlock(cTab)
         Else
            lRet:=.F.
         EndIf 
	  Endif
	  &(aArq[1,2])->(dbSkip())
   Enddo               
   &(aArq[1,2])->(dbCloseArea())
   // -> Verifica se Ocorreram erros
   If Len(aErros) > 0 .and. lRet
      // -> Grava Erros em arquivo de log
      u_xWhiteLog(aErros,"\import\"+cTab+cSegm)
      ShowHelpDlg("Aviso", {"Foi gerado log da importação."},5,{"Favor verificar o arquivo '"+cPath+cTab+cSegm+".log' gerado."},5)
   EndIf   
Return(lRet)




/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³xACTIXXX  ºAutor  ³Márcio A. Zaguetti  º Data ³  02/10/13   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Importação XXX - Tabelas Genericas                          º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Aceleradores TOTVS                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function xACTIXXX(cSegm,cTab)
Local aMsExecAuto:={}
Local cPathArq   :=cTab+".CSV"
Local lRet       :=.T.          
Local nTotRec    :=0
Local nCount     :=0
Private aArq     :={}
Private nErro    :=0
Private aCamp    :={}              
Private cPath    := GetSrvProfString("Startpath","")
Private aErros   := {}                      

   // -> Fecha arquivo temporário
   Processa({|| aArq := AClone(u_xLeCSV(cPathArq,"CSV",";")) },"Agarde, Lendo Arquivo...")
   aCamp:=AClone(aArq[1,3])
   
   If Empty(aArq[1,3])
      If (Select(aArq[1,2]) <> 0)
	     dbSelectArea(aArq[1,2])
		 dbCloseArea()
	  Endif
	  MsgStop("Verifique o arquivo "+Alltrim(cPathArq),"Atenção")
	  lRet:=.F.
	  Return(.F.)
   Endif
                              
   // -> Grava dados Genéricos
   &(aArq[1,2])->(dbGoTop())       
   nTotRec:=&(aArq[1,2])->(LastRec())
   ProcRegua(nTotRec)
   While &(aArq[1,2])->(!Eof()) .and. lRet
      nCount++
            
      IncProc("Processando registro " + AllTrim(Str(nCount)) + " de "+AllTrim(Str(nTotRec)))
      lRet:=.T.
      DbSelectArea("XXX")
      If RecLock("XXX",.T.)
         For z:=1 to Len(aCamp)          
            &(aCamp[z]+":="+aArq[1,2]+"->"+aCamp[z])
         Next z                                                                                  
         MsUnlock("XXX")
      Else
         lRet:=.F.
      EndIf   
	  &(aArq[1,2])->(dbSkip())
   Enddo               
   &(aArq[1,2])->(dbCloseArea())
   // -> Verifica se Ocorreram erros
   If !lRet
      ShowHelpDlg("Erro", {"Ocorreram erros na execução da rotina!"},5,{"Favor verificar os arquivos."},5)
   EndIf   
Return(lRet) 