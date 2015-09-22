/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Atualizacao 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Ativo Fixo 											 !
+------------------+---------------------------------------------------------+
!Nome 			   ! SINDAFEP_ATIVOF_IMPATFX								 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para importação do Ativo Fixo através de arquivo !
!				   ! texto conforme layout padrão Especifico para o SINDAFEP !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima 									 		 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 17/10/2014 											 !
+------------------+---------------------------------------------------------+
! ATUALIZACOES 	   															 !
+-------------------------------------------+-----------+-----------+--------+
! Descricao detalhada da atualizacao 		!Nome do    ! Analista  !Data da !
! 											!Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
!  											!  			! 		 	!		 !
! 											! 		 	! 			! 		 !
+-------------------------------------------+-----------+-----------+--------+
*/

#include "Protheus.ch"

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

//---------------------
//Carrega os parâmetros
//---------------------
User Function IMPATV00()

	//Variáveis
	Private cPerg    := "IMPATV00"
	Private aHelpP01 := {}
	Private aPergs   := {}
	
	//Cria perguntas e help caso não existam
	AADD(aHelpP01, "Informe o local do arquivo para importação.")
	
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Arquivo Importação","Arquivo Importação","Arquivo Importação","mv_ch1","C",60,0,0,"G","","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
		AjustaSx1(cPerg,aPergs)
		
		PutSX1Help("P.IMPATV001.",aHelpP01,,)
	Endif
	
	//Carrega perguntas
	Pergunte(cPerg,.T.)
	
	//Chama função de análise
	If MsgYesNo("Confirma Importação dos Ítens ?","Atenção")
		Processa({||U_xACTISN1()}) 
	Endif 

Return

User Function xACTISN1(cSegm,cTab)
	
	Local aItens     :={}
	Local aAux       :={}
	Local aCab       :={}
	Local aDadosAuto :={} 
	Local nAuxMod    :=nModulo
	Local cFilAux    :=cFilAnt 
	//Local cPathArq   :=cTab+AllTrim(cSegm)+".CSV"
	//Local cPathArq   :="C:\alc\SN2010101.CSV"
	Local cPathArq   := Alltrim(MV_PAR01)
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
	Private lMsHelpAuto := .f. // Determina se as mensagens de help devem ser direcionadas para o arq. de log
	Private lMsErroAuto := .f. // Determina se houve alguma inconsistencia na execucao da rotina 

	//Verifica se existe o arquivo texto
	If !File(cPathArq)
		//Help(" ",1,"NOARQENT")
		Alert("Arquivo não encontrado!")
		Return .F.
	Endif                      

   Processa({|| aArq := AClone(u_xLeCSV(cPathArq,"CSV",";",.T.)) },"Aguarde, Lendo Arquivo...")
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
               //cNomArqErro:= "SN2tmp.log"
               //MostraErro(cPath, cNomArqErro )
               MostraErro()
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
   	  //	MostraErro()
      // -> Grava Erros em arquivo de log
      //u_xWhiteLog(aErros,"\import\"+cTab+cSegm)
      //u_xWhiteLog(aErros,"\import\SN2tmp")
      //ShowHelpDlg("Erro", {"Ocorreram erros na execução da rotina!"},5,{"Favor verificar os erro no arquivo '"+"\import\SN2tmp"+".log' gerado."},5)
   EndIf
      
Return(lRet)

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Programa  ³ xLeCSV  ³ Autor ³  TOTVS               ³ Data ³          ³  ±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Funções específicas TOTVS - Importações                    ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function xLeCSV(cArqCSV,cAliasRet,cTpSepara,lTab)
	
	Local aAlias   := GetArea()
	Local cArqDbf  := Space(0)
	Local nx       := 0
	Local cEol     := CHR(13)+CHR(10)
	Local nHdl     := 0
	Local nTamFile := 0     
	Local nCampos  := 0
	Local nRestaLer:= 0
	Local aDados   := {} 
	Local aCampos  := {} 
	Local aTipos   := {}
	Local aEstrut  := {}
	Local lContinua:=.T.      
	Local cAux     := Space(0)
	Local nAux     := 0
	Local cAuxTipo := Space(0)                  
	Local cMudaVar :=Space(0)
	Local cAuxAlias:=Space(0)
	Local aRet     :={}          
	Local aNotSx3  :={}
	Local cMsn     :=Space(0)
	Local lTabela  :=IIF(lTab == Nil, .F., lTab)
	Local lTabGen  :=.F.
	
	If ValType(cTpSepara) = "U"
	   cTpSepara:= ";"
	Endif
	
	//cArqCSV := "\import\"+Alltrim(cArqCSV)
	cArqCSV := Alltrim(cArqCSV)
	//cArqCSV 
	nHdl    := fOpen(cArqCSV,68)
	
	If nHdl == -1
		MsgStop("O arquivo "+cArqCSV+" não pode ser aberto... Verifique os parâmetros","Atencao")
	    Return(cArqDbf)
	Endif
	
	nTamFile := nRestaLer := fSeek(nHdl,0,2)
	fSeek(nHdl,0,0)
	
	nCampos := 0
	nCount:=0
	While nRestaLer > 0 .and. lContinua
	    nAux      := nAux + 1	
		cLeitura  := fReadStr(nHdl,65535)
	    nRestaLer -= Len(cLeitura)
	    nFimLinha := AT(cEol,cLeitura)+1
	    cString   := Substr(cLeitura,1,nFimLinha)	
	    cString   := Substr(cString,1,Len(cString)-2)
	                                               
	    // -> Armazena Campos
	    If nAux == 1
	       aCampos   := StrToKarr(cString,AllTrim(cTpSepara))
	       nCampos   := Len(aCampos)
	    Else
	       aDados    := StrToKarr(cString,AllTrim(cTpSepara))    
	    EndIf        
	
		// -> Criando arquivo temporário para receber as informações do .CSV
		If Select(cAliasRet) = 0 .and. nAux == 1
			nCampos := IIF(nCampos=0,1,nCampos)
			aEstrut := {}    
	        // -> Cria tabela com os campos da primeira linha da tabela
			For x:=1 to nCampos
			    cAux     :=IIF(Empty(AllTrim(cAux)),aCampos[1],cAux)
			    cAux     :=IIF(SubStr(cAux,3,1)=="_",SubStr(cAux,1,2),SubStr(cAux,1,3))
			    cAuxAlias:=cAux+"_"
				// -> Verifica Campos no Dicionário de Dados
			    If ("XXX" $ cArqCSV)
	               lTabGen:=.T.
	            Else
			       DbSelectArea("SX3")
			       SX3->(DbSetOrder(2))
			       If !SX3->(DbSeek(aCampos[x])) 
	                  AADD(aNotSx3,{aCampos[x],""})
	                  aNotSx3[1,2]+=aCampos[x]+cEol
				      cMsn     :="Campos não encontrados no dicionário de dados -> "
				      lContinua:=.F.
	               EndIf
	            EndIf   
	            If lContinua       
	               // -> Valida código do município
	               If "_COD_MUN" $ AllTrim(aCampos[x])
	                  AADD(aEstrut,{AllTrim(aCampos[x]),TamSx3(aCampos[x])[3],(TamSx3(aCampos[x])[1])+2,TamSx3(aCampos[x])[2]})
	               Else
	   			      AADD(aEstrut,{AllTrim(aCampos[x]),IIF(lTabGen,"C",TamSx3(aCampos[x])[3]),IIF(lTabGen,200,TamSx3(aCampos[x])[1]),IIF(lTabGen,0,TamSx3(aCampos[x])[2])})
	   			   EndIf   
				   If !(SubStr(aCampos[x],1,Len(cAux)) $ cAux) .and. !lTabela
				      lContinua:=.F.
				      cMsn     :="Existem campos diferentes da tabela..."
				      aEstrut  := {}
				      Exit
				   EndIf
				EndIf   
			Next       		                    
			// -> Cria arquivo temporário
			If lContinua
			   cArqDbf := CriaTrab(aEstrut,.t.)
			   Use (cArqDbf) Alias (cAliasRet) Shared New via "dbfcdx"
			   dbselectArea(cAliasRet)
			EndIf			
		Endif
	    
	    // -> Verifica se deu erro
	    cMsn:=IIF(Len(aNotSx3)>0,cMsn+aNotSx3[1,2],cMsn)
	    If !lContinua
	       ShowHelpDlg("Erro", {cMsn},5,{"Favor verificar..."},5)
	       RestArea(aAlias)             
	       aCampos:={}
	       Aadd(aRet,{cArqDbf,cAliasRet,aCampos})
	       Return(aRet)
	    EndIf
	               
	   	// -> Lendo linhas
	    ProcRegua(9999999999)
		For k := 1 To Len(cLeitura)
		
	        nCount++          
	        IncProc("Lendo registro no." + Str(nCount) + " do arquivo.")
			
			If k < 2
			   cString := Substr(cLeitura,k,nFimLinha)	
			Else
			   cString := Substr(cLeitura,k-1,nFimLinha)
		    Endif
		    
	        // -> Ajusta espaços em branco
	        For z:=1 to 1000
	            nx:=At(";;",cString)
	            If nx == 0
	               Exit
	            Else
	               cString:=SubStr(cString,1,nx)+" "+SubStr(cString,nx+1,1000)
	            EndIf
	        Next z        
	        
	        // -> Verifica se a primeira coluna está vazia
	        If SubStr(cString,1,1) == ";"
	           cString:=" "+cString
	        EndIf
	        
	        aDados    := StrToKarr(cString,AllTrim(cTpSepara))      
	        
	//        If Len(aDados) <> Len(aDados)
	//           MsgStop("Erro na estrutura da linha: "+aDados[1]+"/"+aDados[2]+"/"+aDados[3],"Atencao")
	//           fClose(nHdl)
	//           Return("")
	//        EndIf
	        
	        // -> Verifica se a última coluna está vazia
	        If (Len(aDados) - Len(aCampos)) == 1
	           If IIF(lTabGen,"C",TamSx3(aCampos[Len(aCampos)])[3]) == "N"
	              cString:=cString+";0.00"
	           Else
	              cString:=cString+" "
	           EndIf                            admin
	           aDados:={}
	           aDados:=StrToKarr(cString,AllTrim(cTpSepara))
	        EndIf
	                            
	        // -> Grava Ddos            
	        If (Len(aDados) == Len(aCampos))         
	           If !(cAuxAlias $ aDados[1]) 
	
	              dbSelectArea(cAliasRet)
	              RecLock(cAliasRet,.T.)
		    
		          For x := 1 To (Len(aDados)-2)
	                  cAux := aDados[x]
			          // -> Retirando , ou . se numero
	   		          If IIF(lTabGen,"C",TamSx3(aCampos[x])[3]) == "N"
	                     cMudaVar:=Space(0)
	                     For y:=1 To Len(Alltrim(aDados[x]))                                                                       
		                     // -> Se tipo de separdor decimal for ,
		                     If Substr(aDados[x],y,1) = "," 
		                        cMudaVar:=cMudaVar+'.'
		                     Else
		                        cMudaVar:=cMudaVar+Substr(aDados[x],y,1)
		                     Endif                                 
		                 Next y    
			             aDados[x]:=cMudaVar
			          Endif	    
	                     
	                 If IIF(lTabGen,"C",TamSx3(aCampos[x])[3]) == "N"
	                    &(cAliasRet+"->"+AllTrim(aCampos[x])):=IIF( Empty(AllTrim(aDados[x])) ,Val("0"), Val(aDados[x]) )
	                 ElseIf IIF(lTabGen,"C",TamSx3(aCampos[x])[3]) == "D"   
	                    &(cAliasRet+"->"+AllTrim(aCampos[x])):=IIF( Empty(AllTrim(aDados[x])) ,CtoD("  /  /  "), StoD(aDados[x]) )
	                 Else
	                    &(cAliasRet+"->"+AllTrim(aCampos[x])):=aDados[x]              
	                 EndIf   
	
		          Next x
	
		          MsUnLock(cAliasRet)
	
			   EndIf
			EndIf
			k += nFimLinha
			nFimLinha := AT(cEol,SubStr(cLeitura,k))
	
	    Next k
	
	Enddo
	
	RestArea(aAlias)
	
	Aadd(aRet,{cArqDbf,cAliasRet,aCampos})

Return(aRet)                                                            
