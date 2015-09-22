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
!Nome 			   ! GERPL00 												 !
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
! Programa 	! GERPL00	! Autor !Gilson Lima 		 ! Data ! 22/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada no menu para a importação de arquivos			 !
! 			!  																 !
+----------------------------------------------------------------------------*/
User Function GERPL00()

	Private cPerg	:= "GERPL01"
	Private aPergs	:= {}

	Private aDepends:= {}
	Private aTituls	:= {}
	Private aOdonto	:= {}

	Private aDepDef	:= {}
	Private aTitDef	:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Arquivo Dependentes","Arquivo Dependentes","Arquivo Dependentes","mv_ch1","C",60,0,0,"G","","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
		AADD(aPergs,{"Arquivo Titulares","Arquivo Titulares","Arquivo Titulares","mv_ch2","C",60,0,0,"G","","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
		AADD(aPergs,{"Arquivo Odonto","Arquivo Odonto","Arquivo Odonto","mv_ch3","C",60,0,0,"G","","MV_PAR03","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)
	
	Processa({||GERPLDP1()},"Tratando Dependentes...")
	Processa({||GERPLTT1()},"Tratando Titulares...")
	Processa({||GERPLOD1()},"Tratando Odonto...")
	Processa({||GERPLODT()},"Gerando Odonto...")
	Processa({||GERPLPL1()},"Gerando Planos...")
	Processa({||GERPLPL2()},"Gerando Planos...")

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERPLDP1	! Autor !Gilson Lima 		 ! Data ! 22/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Lê os dados do arquivo			 							 !
! 			! de Dependentes												 !
+----------------------------------------------------------------------------*/
Static Function GERPLDP1()

	Local cFileDep 	:= AllTrim(MV_PAR01)
	Local nCount	:= 0
	Local cLinha	:= ''
	Local cSepara	:= '|'
	Local aDados	:= {}
	
	// Verifica se o arquivo pôde ser aberto, se não, encerra o programa
	If !File(cFileDep)
		Alert("Arquivo não encontrado!")
		Return .F.
	EndIf
		
	// Abre o arquivo texto e grava dados
	FT_FUSE(cFileDep)
	FT_FGOTOP()
	
	// Conta os registros
	ProcRegua(RecCount())
	
	While !FT_FEOF()
	
	    nCount++          
        IncProc("Lendo registro nr. " + cValToChar(nCount))

	    cLinha	:= FT_FREADLN()
	    
	    If Len(AllTrim(cLinha)) > 0

		    // Gera Array com dados dos dependentes
		    aDados := StrToKarr(cLinha,AllTrim(cSepara))
		    
		    // Retira Espaços
		    For nA := 1 To Len(aDados)
		    	aDados[nA] := AllTrim(aDados[nA])
		    Next nA
		    
		    // Adiciona ao Array de Dependentes
		    aAdd(aDepends,aDados)

		EndIf
		
		FT_FSKIP()
	EndDo	
	
	// Verifica se algum dependente foi lido do arquivo
	If Len(aDepends) > 0
		Processa({||GERPLDP2()},"Processando dados de Dependentes...")		
	Else
		Alert("Não foi importado nenhum registro.")
	EndIf

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERPLDP2	! Autor !Gilson Lima 		 ! Data ! 22/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Trata e Grava os dados do arquivo	de Dependentes				 !
+----------------------------------------------------------------------------*/
Static Function GERPLDP2()
	
	Local	nCount		:= 0
	Local	cCodigo		:= ''
	Local	lSOS		:= .F.
	Local	lAero		:= .F.
	
	Local aSOSAero	:= {}

	ProcRegua(Len(aDepends))
	
	For nB := 1 To Len(aDepends)
	
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
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERPLTT1	! Autor !Gilson Lima 		 ! Data ! 22/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Lê os dados do arquivo			 							 !
! 			! de Titulares												 !
+----------------------------------------------------------------------------*/
Static Function GERPLTT1()

	Local cFileTit 	:= AllTrim(MV_PAR02)
	Local nCount	:= 0
	Local cLinha	:= ''
	Local cSepara	:= '|'
	Local aDados	:= {}
	
	// Verifica se o arquivo pôde ser aberto, se não, encerra o programa
	If !File(cFileTit)
		Alert("Arquivo não encontrado!")
		Return .F.
	EndIf
		
	// Abre o arquivo texto e grava dados
	FT_FUSE(cFileTit)
	FT_FGOTOP()
	
	// Conta os registros
	ProcRegua(RecCount())
	
	While !FT_FEOF()
	
	    nCount++          
        IncProc("Lendo registro nr. " + cValToChar(nCount))

	    cLinha	:= FT_FREADLN()
	    
	    If Len(AllTrim(cLinha)) > 0

		    // Gera Array com dados dos dependentes
		    aDados := StrToKarr(cLinha,AllTrim(cSepara))
		    
		    // Retira Espaços
		    For nA := 1 To Len(aDados)
		    	aDados[nA] := AllTrim(aDados[nA])
		    Next nA
		    
		    // Adiciona ao Array de Dependentes
		    aAdd(aTituls,aDados)

		EndIf
		
		FT_FSKIP()
	EndDo	
	
	// Verifica se algum titular foi lido do arquivo
	If Len(aTituls) > 0
		Processa({||GERPLTT2()},"Processando dados de Titulares...")		
	Else
		Alert("Não foi importado nenhum registro.")
	EndIf

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERPLTT2	! Autor !Gilson Lima 		 ! Data ! 22/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Trata e Grava os dados do arquivo	de Dependentes				 !
+----------------------------------------------------------------------------*/
Static Function GERPLTT2()
	
	Local	nCount		:= 0
	Local	cCodigo		:= ''
	Local	lSOS		:= .F.
	Local	lAero		:= .F.
	
	Local aSOSAero	:= {}

	ProcRegua(Len(aTituls))
	
	For nB := 1 To Len(aTituls)
	
		lSOS := .F.
		lAero := .F.
		
		If PadL(aTituls[nB][5],2,'0') $ ('09_10_11_12')
			lSOS := .T.
		ElseIf PadL(aTituls[nB][5],2,'0') $ ('01_05')
			lAero := .T.
		EndIf
		
		If lSOS .Or. lAero
			
			nPosAdic := 0
			nPos 	 := 0
			
			For nD := 1 To Len(aSOSAero)
				bSeek := {|x| x == aTituls[nB][1]}
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
					aAdd(aSOSAero,{aTituls[nB][1],'S',''})
				ElseIf lAero
					aAdd(aSOSAero,{aTituls[nB][1],'','S'})
				EndIf 
			EndIf
		EndIf

		// Verifica se já foi incluído no Array Definitivo
		nPosDef := 0
		nPos	:= 0
		For nC := 1 To Len(aTitDef)
			bSeek2 := {|x| x == aTituls[nB][1]}
			nPos := aScan(aTitDef[nC],bSeek2)
			If nPos > 0
				nPosDef := nC
			EndIf
		Next nC

		If nPosDef > 0
			Loop
		Else
			aAdd(aTitDef,aTituls[nB])
		EndIf
	
	Next nB
	
	For nE := 1 To Len(aTitDef)
		
		nPosAero := 0
		nPos	 := 0
		
		For nF := 1 To Len(aSOSAero)
			bSeek3 := {|x| x == aTitDef[nE][1]}
			nPos := aScan(aSOSAero[nF],bSeek3)
			
			If nPos > 0
				nPosAero := nF
				Exit
			EndIf
		Next nF
		
		If nPosAero > 0
			If aSOSAero[nPosAero][2] == 'S'
				aAdd(aTitDef[nE],'S')
			Else
				aAdd(aTitDef[nE],'N')
			EndIf
			
			If aSOSAero[nPosAero][3] == 'S'
				aAdd(aTitDef[nE],'S')
			Else
				aAdd(aTitDef[nE],'N')
			EndIf
		Else
			aAdd(aTitDef[nE],'N')
			aAdd(aTitDef[nE],'N')
		EndIf
		
	Next nE
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERPLOD1	! Autor !Gilson Lima 		 ! Data ! 22/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Lê os dados do arquivo de Odonto								 !
+----------------------------------------------------------------------------*/
Static Function GERPLOD1()

	Local cFileOdo 	:= AllTrim(MV_PAR03)
	Local nCount	:= 0
	Local cLinha	:= ''
	Local cSepara	:= '|'
	Local aDados	:= {}

	// Verifica se o arquivo pôde ser aberto, se não, encerra o programa
	If !File(cFileOdo)
		Alert("Arquivo não encontrado!")
		Return .F.
	EndIf
		
	// Abre o arquivo texto e grava dados
	FT_FUSE(cFileOdo)
	FT_FGOTOP()
	
	// Conta os registros
	ProcRegua(RecCount())
	
	While !FT_FEOF()
	
	    nCount++          
        IncProc("Lendo registro nr. " + cValToChar(nCount))

	    cLinha	:= FT_FREADLN()
	    
	    If Len(AllTrim(cLinha)) > 0

		    // Gera Array com dados dos dependentes
		    aDados := StrToKarr(cLinha,AllTrim(cSepara))
		    
		    // Retira Espaços
		    For nA := 1 To Len(aDados)
		    	aDados[nA] := AllTrim(aDados[nA])
		    Next nA
		    
		    // Adiciona ao Array de Dependentes
		    aAdd(aOdonto,aDados)

		EndIf
		
		FT_FSKIP()
	EndDo	

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERPLODT	! Autor !Gilson Lima 		 ! Data ! 22/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Trata e Grava os dados dos planos Odonto						 !
+----------------------------------------------------------------------------*/
Static Function GERPLODT()

	Local nFamOdont	:= 0
	Local cCodGrp	:= '000002'
	
	
	For nA := 1 To Len(aOdonto)
	
		If aOdonto[nA][2] == 'NULL'

			cCodTit	  := Posicione("SA1",10,xFilial("SA1")+PadR(aOdonto[nA][1],6,' '),"A1_COD")

			If AllTrim(cCodTit) == ''
				Loop
			EndIf
			
			nFamOdont++
			cFamOdont := 'ODT'+PadL(cValToChar(nFamOdont),7,'0')
			dDtNasc	  := Posicione("SA1",10,xFilial("SA1")+PadR(aOdonto[nA][1],6,' '),"A1_DTNASC")
			cCodPlan  := ''
						
			Do Case
				Case aOdonto[nA][3] == '6'
					cCodPlan := 'DENT001'
				Case aOdonto[nA][3] == '7'
					cCodPlan := 'DENT002'
				Case aOdonto[nA][3] == '8'
					cCodPlan := 'DENT003'
			EndCase

			dbSelectArea("SZH")
			
				RecLock("SZH",.T.)
				
					SZH->ZH_FILIAL	:= xFilial("SZH")
					SZH->ZH_LOJA	:= '01'
					SZH->ZH_FAMILIA	:= cFamOdont
					SZH->ZH_CODGRP	:= cCodGrp
					SZH->ZH_CODTIT	:= cCodTit
					SZH->ZH_DTNASC	:= dDtNasc
					
					SZH->ZH_CODPLAN	:= cCodPlan
					SZH->ZH_DATAINI	:= CtoD(aOdonto[nA][4])
					
					If aOdonto[nA][5] != 'NULL' .And. AllTrim(aOdonto[nA][5]) != ''
						SZH->ZH_DATATER := CtoD(aOdonto[nA][5])
					EndIf
				
				SZH->(MsUnLock())
			SZH->(dbCloseArea())
		
		EndIf
	
	Next nA
	
	For nB := 1 To Len(aOdonto)
	
		If aOdonto[nB][2] != 'NULL'
		
			cCodTit		:= Posicione("SA1",10,xFilial("SA1")+PadR(aOdonto[nB][1],6,' '),"A1_COD")
			
			If AllTrim(cCodTit) == ''
				Loop
			EndIf			
			
			cFamilia	:= Posicione("SZH",6,xFilial("SZH")+'000002'+cCodTit,"ZH_FAMILIA")

			If AllTrim(cFamilia) == ''
				Loop
			EndIf			
			
			cCodDep		:= Posicione("SZG",4,xFilial("SZG")+PadR(aOdonto[nB][2],6,' '),"ZG_CODIGO")
			dDtNasc		:= Posicione("SZG",4,xFilial("SZG")+PadR(aOdonto[nB][2],6,' '),"ZG_DTNASC")
			
			For nC := 1 To Len(aDepDef)
				If aDepDef[nC][1] == aOdonto[nB][2]
					If aDepDef[nC][3] == 'NULL'
						cGrauPar := '000002'	// Filho
					Else
						cGrauPar := PadL(aDepDef[nC][3],6,'0')
					EndIf
				EndIf
			Next nC
			
			Do Case
				Case aOdonto[nB][3] == '6'
					cCodPlan := 'DENT001'
				Case aOdonto[nB][3] == '7'
					cCodPlan := 'DENT002'
				Case aOdonto[nB][3] == '8'
					cCodPlan := 'DENT003'
			EndCase
			
			dbSelectArea("SZJ")
			
				RecLock("SZJ",.T.)
				
					SZJ->ZJ_FILIAL		:= xFilial("SZJ")
					SZJ->ZJ_FAMILIA		:= cFamilia
					SZJ->ZJ_CODIGO		:= cCodDep
					SZJ->ZJ_DTNASC		:= dDtNasc
					SZJ->ZJ_CODPAR		:= cGrauPar
					SZJ->ZJ_CODPLAN		:= cCodPlan
					SZJ->ZJ_DATAINI		:= CtoD(aOdonto[nB][4])

					If aOdonto[nB][5] != 'NULL' .And. AllTrim(aOdonto[nB][5]) != ''
						SZJ->ZJ_DATATER := CtoD(aOdonto[nB][5])
					EndIf
					
				SZJ->(MsUnLock())
			
			SZJ->(dbCloseArea())
		
		EndIf
	
	Next nB
	
	Alert('Concluído!')

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERPLPL1	! Autor !Gilson Lima 		 ! Data ! 23/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Trata e Grava os dados dos Planos Saúde						 !
+----------------------------------------------------------------------------*/
Static Function GERPLPL1()

	Local cCodGrp	:= '000001'	// Unimed
	
	For nA := 1 To Len(aTitDef)
	
		cCodTit := Posicione("SA1",10,xFilial("SA1")+PadR(aTitDef[nA][1],6,' '),"A1_COD")
		
		If AllTrim(cCodTit) == ''
			Loop
		EndIf
		
		dbSelectArea("SZH")
			
			RecLock("SZH",.T.)
		
				SZH->ZH_FILIAL		:= xFilial("SZH")
				SZH->ZH_LOJA		:= '01'
				SZH->ZH_FAMILIA		:= PadL(aTitDef[nA][2],7,'0')
				SZH->ZH_CODGRP		:= cCodGrp
				SZH->ZH_CODTIT		:= cCodTit
				SZH->ZH_DTNASC		:= CtoD(aTitDef[nA][3])
				SZH->ZH_CODPLAN		:= aTitDef[nA][4]
				SZH->ZH_SOS			:= aTitDef[nA][9]
				SZH->ZH_AERO		:= aTitDef[nA][10]
				
				If AllTrim(aTitDef[nA][6])!= ''
					SZH->ZH_CARTEIR := aTitDef[nA][6]
				EndIf
				
				SZH->ZH_DATAINI		:= CtoD(aTitDef[nA][7])

				If aTitDef[nA][8] != 'NULL' .And. AllTrim(aTitDef[nA][8]) != ''
					SZH->ZH_DATATER := CtoD(aTitDef[nA][8])
				EndIf				
				
			SZH->(MsUnLock())
		SZH->(dbCloseArea())
	
	Next nA

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERPLPL2	! Autor !Gilson Lima 		 ! Data ! 23/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Trata e Grava os dados dos Planos Saúde - Dependentes			 !
+----------------------------------------------------------------------------*/
Static Function GERPLPL2()
	
	For nA := 1 To Len(aDepDef)
		
		cCodTit		:= Posicione("SA1",10,xFilial("SA1")+PadR(aDepDef[nA][2],6,' '),"A1_COD")
		
		If AllTrim(cCodTit) == ''
			Loop
		EndIf

		cFamilia	:= Posicione("SZH",6,xFilial("SZH")+'000001'+cCodTit,"ZH_FAMILIA")

		If AllTrim(cFamilia) == ''
			Loop
		EndIf			
		
		cCodDep		:= Posicione("SZG",4,xFilial("SZG")+PadR(aDepDef[nA][1],6,' '),"ZG_CODIGO")
		
		If AllTrim(cCodDep) == ''
			Loop
		EndIf
		
		dDtNasc		:= Posicione("SZG",4,xFilial("SZG")+PadR(aDepDef[nA][1],6,' '),"ZG_DTNASC")
		
		If aDepDef[nA][3] == 'NULL'
			cGrauPar := '000002'	// Filho
		Else
			cGrauPar := PadL(aDepDef[nA][3],6,'0')
		EndIf

		dbSelectArea("SZJ")
		
			RecLock("SZJ",.T.)
			
				SZJ->ZJ_FILIAL		:= xFilial("SZJ")
				SZJ->ZJ_FAMILIA		:= cFamilia
				SZJ->ZJ_CODIGO		:= cCodDep
				SZJ->ZJ_DTNASC		:= dDtNasc
				SZJ->ZJ_CODPAR		:= cGrauPar
				SZJ->ZJ_CODPLAN		:= aDepDef[nA][5]
				SZJ->ZJ_SOS			:= aDepDef[nA][9]
				SZJ->ZJ_AERO		:= aDepDef[nA][10]
				
				If aDepDef[nA][6] != 'NULL' .And. AllTrim(aDepDef[nA][6]) != ''
					SZJ->ZJ_CARTEIR	:= aDepDef[nA][6]
				EndIf
				
				SZJ->ZJ_DATAINI		:= CtoD(aDepDef[nA][7])

				If aDepDef[nA][8] != 'NULL' .And. AllTrim(aDepDef[nA][8]) != ''
					SZJ->ZJ_DATATER := CtoD(aDepDef[nA][8])
				EndIf
				
			SZJ->(MsUnLock())
		
		SZJ->(dbCloseArea())
		
	Next nA

	Alert('OK')
Return