/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Atualização 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Financeiro 											 !
+------------------+---------------------------------------------------------+
!Nome 			   ! IMPDEP01 												 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Importação do arquivo com dados de dependentes			 !
!				   !  			 									 		 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 19/01/15 												 !
+------------------+---------------------------------------------------------+
! ATUALIZACOES 	   															 !
+-------------------------------------------+-----------+-----------+--------+
! Descricao detalhada da atualizacao 		!Nome do    ! Analista  !Data da !
! 											!Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
! 									 		! 		 	! 		 	!		 !
! 											! 		 	! 			! 		 !
+-------------------------------------------+-----------+-----------+--------*/
#include "Protheus.ch"

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! IMPDEP01	! Autor !Gilson Lima 		 ! Data ! 16/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada no menu para a importação do arquivo			 !
! 			! de Dependentes												 !
+----------------------------------------------------------------------------*/
User Function IMPDEP01()

	Private cPerg	:= "IMPDEP01"
	Private aPergs	:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Arquivo para Importação","Arquivo Importação","Arquivo Importação","mv_ch1","C",60,0,0,"G","","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)
	
	// Chama a função de importação
	If MsgYesNo("Confirma importação do arquivo?","Atenção")
		Processa({||IMPDEP02()},"Lendo Arquivo de Dependentes...")
	EndIf
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! IMPDEP02	! Autor !Gilson Lima 		 ! Data ! 19/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Lê os dados do arquivo			 							 !
! 			! de Dependentes												 !
+----------------------------------------------------------------------------*/
Static Function IMPDEP02()

	Local nLin		:= 0
	Local cFile		:= ''
	Local cLinha	:= ''
	Local aDepends  := {}
	Local aDados	:= {}
	Local cSepara	:= '|'
	Local nHdl		:= 0
	Local nTamFile	:= 0
	Local nLidos	:= 0
	Local nRestaLer	:= 0
	Local lContinua := .T.
	Local cEol      := CHR(13)+CHR(10)
	Local cLeitura	:= ''
	Local nFimLinha	:= 0
	Local nCount	:= 0
	Local nNrLinhas	:= 0
	Local cString	:= ''

	// Verifica se existe o arquivo texto
	cFile := AllTrim(MV_PAR01)
	
	// Abre o arquivo
	nHdl := fOpen(cFile,68)
	
	// Verifica se o arquivo pôde ser aberto, se não, encerra o programa
	If nHdl == -1
		MsgStop("O arquivo "+cFile+" não pode ser aberto... Verifique os parâmetros","Atencao")
	    Return
	Endif	
	
	// Determina o tamanho do arquivo
	nTamFile := nRestaLer := fSeek(nHdl,0,2)
	
	// Posiciona no início do arquivo
	fSeek(nHdl,0,0)
	
    ProcRegua(2200)
	
	While nRestaLer > 0 .And. lContinua
	
	    nCount++          
        IncProc("Lendo registro nr. " + cValToChar(nCount))
		
		// Posiciona o arquivo após os dados lidos
		fSeek(nHdl,nLidos,0)
		
		// Variável com o conteúdo lido
		cLeitura  	:= fReadStr(nHdl,65535)
		
		// Posição do final de linha
		nFimLinha 	:= AT(cEol,cLeitura)+1
		
		// Verifica se é a última linha do arquivo
		If nFimLinha == 1
			cString	:= cLeitura
		Else
			cString	:= Substr(cLeitura,1,nFimLinha)
		EndIf
		
		nLidos		+= Len(cString)	
	    nRestaLer 	-= Len(cString)
	    
	    // Se não for última linha
	   	If nFimLinha > 1
	    	cString	:= Substr(cString,1,Len(cString)-2)
	    EndIf
	    
	    // Gera Array com dados dos dependentes
	    aDados   	:= StrToKarr(cString,AllTrim(cSepara))
	    
	    // Retira Espaços
	    For nA := 1 To Len(aDados)
	    	aDados[nA] := AllTrim(aDados[nA])
	    Next nA
	    
	    // Adiciona ao Array de Dependentes
	    aAdd(aDepends,aDados)
	
	End
	
	// Verifica se algum dependente foi lido do arquivo
	If Len(aDepends) > 0
		Processa({||IMPDEP03(aDepends)},"Processando dados de Dependentes...")		
	Else
		Alert("Não foi importado nenhum registro.")
	EndIf

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! IMPDEP03	! Autor !Gilson Lima 		 ! Data ! 19/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Trata e Grava os dados do arquivo	de Dependentes				 !
+----------------------------------------------------------------------------*/
Static Function IMPDEP03(aDepends)
	
	Local	nCount		:= 0
	Local	cCodigo		:= ''
	Local	lSOS		:= .F.
	Local	lAero		:= .F.
	
	Local aDepDef	:= {}
	Local aSOSAero	:= {}

	ProcRegua(Len(aDepends))
	
	For nB := 1 To Len(aDepends)
	
		/*
		lSOS := .F.
		lAero := .F.
		
		If PadL(aDepends[nB][4],2,'0') $ ('09_10_11_12')
			lSOS := .T.
		ElseIf PadL(aDepends[nB][4],2,'0') $ ('01_05')
			lAero := .T.
		EndIf
		
		If lSOS .Or. lAero
			
			nPosAdic := 0
			nPos 	 := 0
			
			For nD := 1 To Len(aSOSAero)
				bSeek := {|x| x == aDepends[nB][1]}
				nPos := aScan(aSOSAero[nD],bSeek)
				If nPos > 0
					nPosAdic := nD
					Exit
				EndIf
			Next nD
			
			If nPosAdic > 0
				If lSOS
					aSOSAero[nPosAdic][2] := 'S'
				ElseIf lAero
					aSOSAero[nPosAdic][3] := 'S'
				EndIf 
			Else
				If lSOS
					aAdd(aSOSAero,{aDepends[nB][1],'S',''})
				ElseIf lAero
					aAdd(aSOSAero,{aDepends[nB][1],'','S'})
				EndIf 
			EndIf
		EndIf
		*/
		
		// Verifica se já foi incluído no Array Definitivo
		nPosDef := 0
		nPos	:= 0
		For nC := 1 To Len(aDepDef)
			bSeek2 := {|x| x == aDepends[nB][1]}
			nPos := aScan(aDepDef[nC],bSeek2)
			If nPos > 0
				nPosDef := nC
			EndIf
		Next nC

		If nPosDef > 0
			Loop
		Else
			aAdd(aDepDef,aDepends[nB])
		EndIf
	
	Next nB
	
	/*
	For nE := 1 To Len(aDepDef)
		
		nPosAero := 0
		nPos	 := 0
		
		For nF := 1 To Len(aSOSAero)
			bSeek3 := {|x| x == aDepDef[nE][1]}
			nPos := aScan(aSOSAero[nF],bSeek3)
			
			If nPos > 0
				nPosAero := nF
				Exit
			EndIf
		Next nF
		
		If nPosAero > 0
			If aSOSAero[nPosAero][2] == 'S'
				aAdd(aDepDef[nE],'S')
			Else
				aAdd(aDepDef[nE],'N')
			EndIf
			
			If aSOSAero[nPosAero][3] == 'S'
				aAdd(aDepDef[nE],'S')
			Else
				aAdd(aDepDef[nE],'N')
			EndIf
		Else
			aAdd(aDepDef[nE],'N')
			aAdd(aDepDef[nE],'N')
		EndIf
		
	Next nE
	*/
	If Len(aDepDef) > 0
		
	EndIf

	// Verifica se algum dependente foi lido do arquivo
	If Len(aDepDef) > 0
		Processa({||IMPDEP04(aDepDef)},"Gravando dados de Dependentes...")		
	Else
		Alert("Não foi importado nenhum registro.")
	EndIf

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! IMPDEP04	! Autor !Gilson Lima 		 ! Data ! 19/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Trata e Grava os dados do arquivo	de Dependentes				 !
+----------------------------------------------------------------------------*/
Static Function IMPDEP04(aDepends)

	Local	nCount		:= 0
	Local	cCodigo		:= ''
	
	For nA := 1 To Len(aDepends)
		
		nCount++
		IncProc("Gravando Dependente " + cValToChar(nCount))
		
		cCodigo	:= GetSXEnum("SZG","ZG_CODIGO")
		cTitular := Posicione("SA1",10,xFilial("SA1")+PadR(aDepends[nA][2],6,' '),"A1_COD")
		
		dbSelectArea("SZG")
		
			RecLock("SZG",.T.)
		
				SZG->ZG_FILIAL		:= xFilial("SZG")
				SZG->ZG_CODIGO		:= cCodigo
				SZG->ZG_CDANT		:= PadR(aDepends[nA][1],6,' ')
				SZG->ZG_CDTTANT		:= PadR(aDepends[nA][2],6,' ')
				SZG->ZG_NOME		:= Upper(Substr(RetGraf(aDepends[nA][5]),1,40))
				
				If AllTrim(cTitular) != ''
					SZG->ZG_CODTIT	:= cTitular
				EndIf
				
				/*
				If AllTrim(aDepends[nA][9]) != '' .And. aDepends[nA][9] != 'NULL'
					SZG->ZG_REGNASC	:= Substr(aDepends[nA][9],1,30)
				EndIf
				*/	

				If AllTrim(aDepends[nA][6]) != '' .And. aDepends[nA][6] != 'NULL'
					If CGC(aDepends[nA][6])
						SZG->ZG_CPF	:= aDepends[nA][6]
					EndIf
				EndIf	
				/*
				If AllTrim(aDepends[nA][11]) != '' .And. aDepends[nA][11] != 'NULL'
					SZG->ZG_RG	:= aDepends[nA][11]
				EndIf
				*/	

				If AllTrim(aDepends[nA][7]) != '' .And. aDepends[nA][7] != 'NULL'
					SZG->ZG_DTNASC	:= CtoD(aDepends[nA][7])
				EndIf	

				If AllTrim(aDepends[nA][8]) != '' .And. aDepends[nA][8] $ ('MF')
					SZG->ZG_SEXO	:= Upper(aDepends[nA][8])
				EndIf	

				If AllTrim(aDepends[nA][9]) != '' .And. aDepends[nA][9] != 'NULL'
					SZG->ZG_NOMEMAE	:= Upper(Substr(RetGraf(aDepends[nA][9]),1,40))
				EndIf	

				If AllTrim(aDepends[nA][4]) != '' .And. aDepends[nA][4] != 'NULL'
					SZG->ZG_OBS	:= Upper(Substr(RetGraf(aDepends[nA][4]),1,250))
				EndIf

			SZG->(MsUnLock())

		ConfirmSX8()			
		
		SZG->(dbCloseArea())
		
	Next
	
	Alert("Importação de Dependentes efetuada com sucesso.")

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RetGraf	! Autor !Gilson Lima 		 ! Data ! 15/10/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Função para retirar os Caracteres Especiais do texto			 !
!			!																 !
+----------------------------------------------------------------------------*/
Static Function RetGraf(_sOrig)

   local _sRet := _sOrig

   _sRet = StrTran (_sRet, "á", "a")
   _sRet = StrTran (_sRet, "é", "e")
   _sRet = StrTran (_sRet, "í", "i")
   _sRet = StrTran (_sRet, "ó", "o")
   _sRet = StrTran (_sRet, "ú", "u")
   _sRet = StrTran (_sRet, "Á", "A")
   _sRet = StrTran (_sRet, "É", "E")
   _sRet = StrTran (_sRet, "Í", "I")
   _sRet = StrTran (_sRet, "Ó", "O")
   _sRet = StrTran (_sRet, "Ú", "U")
   _sRet = StrTran (_sRet, "ã", "a")
   _sRet = StrTran (_sRet, "õ", "o")
   _sRet = StrTran (_sRet, "Ã", "A")
   _sRet = StrTran (_sRet, "Õ", "O")
   _sRet = StrTran (_sRet, "â", "a")
   _sRet = StrTran (_sRet, "ê", "e")
   _sRet = StrTran (_sRet, "î", "i")
   _sRet = StrTran (_sRet, "ô", "o")
   _sRet = StrTran (_sRet, "û", "u")
   _sRet = StrTran (_sRet, "Â", "A")
   _sRet = StrTran (_sRet, "Ê", "E")
   _sRet = StrTran (_sRet, "Î", "I")
   _sRet = StrTran (_sRet, "Ô", "O")
   _sRet = StrTran (_sRet, "Û", "U")
   _sRet = StrTran (_sRet, "ç", "c")
   _sRet = StrTran (_sRet, "Ç", "C")
   _sRet = StrTran (_sRet, "à", "a")
   _sRet = StrTran (_sRet, "À", "A")
   _sRet = StrTran (_sRet, "º", ".")
   _sRet = StrTran (_sRet, "ª", ".")
   _sRet = StrTran (_sRet, chr (9), " ") // TAB
   
Return _sRet