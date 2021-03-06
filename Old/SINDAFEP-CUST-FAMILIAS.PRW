/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Manuten��o 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Plano de Sa�de											 !
+------------------+---------------------------------------------------------+
!Nome 			   ! FAMILIAS 												 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Manuten��o de Fam�lias dos Planos e centralizador das   !
!				   ! Fun��es de manuten��o da customiza��o					 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 19/11/2014												 !
+------------------+---------------------------------------------------------+
! ATUALIZACOES 	   															 !
+-------------------------------------------+-----------+-----------+--------+
! Descricao detalhada da atualizacao 		!Nome do    ! Analista  !Data da !
! 											!Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
! 											! 			! 			! 		 !
! 											! 			! 			! 		 !
+-------------------------------------------+-----------+-----------+--------*/

#Include "Protheus.ch"

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! PSFAM00   ! Autor !Gilson Lima 		 ! Data ! 19/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Tela de Fam�lias												 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function PSFam00()

	Local cAlias	:= "SZH"
	
	Private cCadastro	:= "Manuten��o de Planos"
	Private aRotina		:= {}
		
	Private bCampo		:= {|nField| FieldName(nField)}
	
	Private aSize 	:= {}
	Private aInfo 	:= {}
	Private aObj	:= {}
	Private aPObj	:= {}
	Private aPGet	:= {}
	
	// Retorna a �rea �til das janelas Protheus
	aSize := MsAdvSize()
	
	aAdd(aObj,{100,160,.T.,.F.})
	aAdd(aObj,{100,100,.T.,.T.})
	//aAdd(aObj,{100,015,.T.,.F.})
	
	// C�lculo autom�tico das dimens�es dos objetos (altura/largura) em pixel
	aInfo := {aSize[1], aSize[2], aSize[3], aSize[4], 3, 3}
	aPObj := MsObjSize(aInfo,aObj)
	
	// C�lculo autom�tico de dimens�es dos objeto MSGet
	aPGet := MsObjGetPos((aSize[3] - aSize[1]), 315, {{004, 024, 240, 270}})
	
	aAdd(aRotina,{"Pesquisar"				,"AxPesqui"   ,0,1})
	aAdd(aRotina,{"Visualizar"				,"U_PSFamMan" ,0,2})
	aAdd(aRotina,{"Incluir"					,"U_PSFamInc" ,0,3})
	aAdd(aRotina,{"Alterar"					,"U_PSFamMan" ,0,4})
	aAdd(aRotina,{"Excluir"					,"U_PSFamMan" ,0,5})
	/*aAdd(aRotina,{"Grupos"					,"U_AXCADSZD" ,0,6})
	aAdd(aRotina,{"Planos"					,"U_AXCADSZE" ,0,7})
	aAdd(aRotina,{"Descr. Faixas Et�rias"	,"U_AXCADSZK" ,0,8})
	aAdd(aRotina,{"Faixas Et�rias"			,"U_AXCADSZF" ,0,9})
	aAdd(aRotina,{"Parentescos"				,"U_AXCADSZI" ,0,10})
	aAdd(aRotina,{"Dependentes"				,"U_AXCADSZG" ,0,11})*/

	dbSelectArea(cAlias)
	(cAlias)->(dbSetOrder(1))
	(cAlias)->(dbGoTop())
	
	mBrowse(,,,,cAlias)

Return Nil

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! PSFamInc  ! Autor !Gilson Lima 		 ! Data ! 19/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao de Inclus�o do Cadastro de Fam�lias					 !
+----------------------------------------------------------------------------*/
User Function PSFamInc(cAlias,nReg,nOpc)
	
	Local oDlg
	Local oGet
	Local nX	:= 0
	Local nOpcA	:= 0
	
	Private aHeader	:= {}
	Private aCols	:= {}
	Private aGets	:= {}
	Private aTela	:= {}
	
	dbSelectArea(cAlias)
	(cAlias)->(dbSetOrder(1))
	
	For nX := 1 To FCount()
		M->&(Eval(bCampo,nX)) := CriaVar(FieldName(nX),.T.)
	Next nX
	
	GeraHeader()
	GeraCols(nOpc)
	
	DEFINE MSDIALOG oDlg TITLE cCadastro FROM aSize[7],aSize[1] TO aSize[6],aSize[5] OF oMainWnd PIXEL
	
		EnChoice(cAlias, nReg, nOpc,,,,, aPObj[1])
		
		oGet := MsGetDados():New(aPObj[2,1], aPObj[2,2], aPObj[2,3], aPObj[2,4], nOpc, "U_LinOk(n)", ".T.",, .T.)
		
	ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{|| IIF(TudoOk() .And. Obrigatorio(aGets, aTela), (nOpcA := 1, oDlg:End()), NIL)},{|| oDlg:End()})
	
	If nOpcA == 1 .And. nOpc == 3
		Grava(nOpc)
	EndIf

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! PSFamMan  ! Autor !Gilson Lima 		 ! Data ! 21/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao de Manuten��o do Cadastro de Fam�lias					 !
+----------------------------------------------------------------------------*/
User Function PSFamMan(cAlias,nReg,nOpc)

	Local oDlg
	Local oGet
	Local nOpcA	:= 0
	
	Private aHeader	:= {}
	Private aCols	:= {}
	Private aGets	:= {}
	Private aTela	:= {}
	Private aREG	:= {}
	
	dbSelectArea(cAlias)
	(cAlias)->(dbSetOrder(1))
	
	For nX := 1 To FCount()
		M->&(Eval(bCampo, nX)) := FieldGet(nX)
	Next nX
	
	GeraHeader()
	GeraCols(nOpc)
	
	DEFINE MSDIALOG oDlg TITLE cCadastro FROM aSize[7],aSize[1] TO aSize[6],aSize[5] OF oMainWnd PIXEL
	
		EnChoice(cAlias, nReg, nOpc,,,,, aPObj[1])
		
		oGet := MsGetDados():New(aPObj[2,1], aPObj[2,2], aPObj[2,3], aPObj[2,4], nOpc, "U_LinOk(n)", ".T.",, .T.,,,,,,,,"U_DelOk(n)")
		
	ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{|| IIF(TudoOk() .And. Obrigatorio(aGets, aTela), (nOpcA := 1, oDlg:End()), NIL)},{|| oDlg:End()})

	If nOpcA == 1 .And. (nOpc == 4 .Or. nOpc == 5)
		Grava(nOpc, aREG)
	EndIf	
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GeraHeader! Autor !Gilson Lima 		 ! Data ! 21/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao para cria��o do vetor aHeader da Grid					 !
+----------------------------------------------------------------------------*/
Static Function GeraHeader()

	Local aArea := GetArea()
	
	Local aCamposSZJ := {} // Campos da tabela de Integrantes
	
	// Campos a exibir da tabela de Integrantes - SZJ	
	aAdd(aCamposSZJ,"ZJ_CODIGO")
	aAdd(aCamposSZJ,"ZJ_NOME")
	aAdd(aCamposSZJ,"ZJ_DTNASC")
	aAdd(aCamposSZJ,"ZJ_IDADE")
	aAdd(aCamposSZJ,"ZJ_SEXO")
	aAdd(aCamposSZJ,"ZJ_REGNASC")
	aAdd(aCamposSZJ,"ZJ_CPF")
	aAdd(aCamposSZJ,"ZJ_RG")
	aAdd(aCamposSZJ,"ZJ_CODPAR")
	aAdd(aCamposSZJ,"ZJ_DESCPAR")
	aAdd(aCamposSZJ,"ZJ_NOMEMAE")
	aAdd(aCamposSZJ,"ZJ_CODPLAN")
	aAdd(aCamposSZJ,"ZJ_DESPLAN")
	aAdd(aCamposSZJ,"ZJ_SOS")
	aAdd(aCamposSZJ,"ZJ_AERO")
	aAdd(aCamposSZJ,"ZJ_CARTEIR")
	aAdd(aCamposSZJ,"ZJ_FAIXAET")
	aAdd(aCamposSZJ,"ZJ_DATAINI")
	aAdd(aCamposSZJ,"ZJ_INC24H")
	aAdd(aCamposSZJ,"ZJ_DATATER")
	aAdd(aCamposSZJ,"ZJ_OBS")
	
	dbSelectArea("SX3")
	SX3->(dbSetOrder(1))
	SX3->(dbGoTop())

	SX3->(dbSeek("SZJ"))
	While SX3->(!EOF()) .And. X3_ARQUIVO == "SZJ"
		For nZ := 1 To Len(aCamposSZJ)
			If X3Uso(X3_USADO) .And. cNivel >= X3_NIVEL .And. Trim(X3_CAMPO) == aCamposSZJ[nZ]
				aAdd(aHeader,{Trim(X3TITULO()),;
					X3_CAMPO,;
					X3_PICTURE,;
					X3_TAMANHO,;
					X3_DECIMAL,;
					X3_VALID,;
					X3_USADO,;
					X3_TIPO,;
					X3_ARQUIVO,;
					X3_CONTEXT})
				Exit
			EndIf
		Next nZ
		SX3->(dbSkip())
	End
	
	RestArea(aArea)
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GeraCols	! Autor !Gilson Lima 		 ! Data ! 21/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao gera��o da linhas da Grid								 !
+----------------------------------------------------------------------------*/
Static Function GeraCols(nOpc)

	Local aArea  := GetArea()
	Local cChave := ""
	Local cAlias := "SZJ"
	
	If nOpc <> 3
		
		cChave := xFilial(cAlias)+SZH->ZH_FAMILIA
		
		dbSelectArea(cAlias)
		(cAlias)->(dbSetOrder(1))
		(cAlias)->(dbGoTop())
		(cAlias)->(dbSeek(cChave))
		
		While (cAlias)->(!EOF()) .And. SZJ->(ZJ_FILIAL+ZJ_FAMILIA) == cChave .And. (cAlias)->(ZJ_CODPAR) != "000000" // Codigo de Parentesco != Titular
			
			aAdd(aReg, SZJ->(RecNo()))
			aAdd(aCols, Array(Len(aHeader)+1))
			
			For nI := 1 To Len(aHeader)
				If aHeader[nI,10] == "V"
					aCols[Len(aCols),nI] := CriaVar(aHeader[nI,2],.T.)
				Else
					aCols[Len(aCols),nI] := FieldGet(FieldPos(aHeader[nI,2]))
				EndIf
			Next nI
			
			aCols[Len(aCols),Len(aHeader)+1] := .F.
			
			(cAlias)->(dbSkip())
		End
	Else
		aAdd(aCols, Array(Len(aHeader) + 1))
		
		For nI := 1 To Len(aHeader)
			aCols[1,nI] := CriaVar(aHeader[nI, 2], .T.)
		Next nI
		
		aCols[1, Len(aHeader)+1] := .F.
	EndIf
	
	RestArea(aArea)
	
return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! Grava		! Autor !Gilson Lima 		 ! Data ! 21/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao para grava��o dos dados nas tabelas					 !
+----------------------------------------------------------------------------*/
Static Function Grava(nOpc,aAltera)

	Local cChave := ""

	If nOpc == 3 	// Inclus�o
	
		// Grava Itens
		
		If AllTrim(aCols[1][1]) != ''		
		
			// Tabela SZG = Dependentes
			dbSelectArea("SZG")
			SZG->(dbSetOrder(1))
			
			For nX := 1 To Len(aCols)
			
				cChave := xFilial("SZG")+aCols[nX,GdFieldPos("ZJ_CODIGO")]
				
				SZG->(dbSeek(cChave))
				
				RecLock("SZG",.F.)
				
					SZG->ZG_FILIAL	:= xFilial("SZH")
					SZG->ZG_NOME	:= aCols[nX,GdFieldPos("ZJ_NOME")]
					SZG->ZG_REGNASC	:= aCols[nX,GdFieldPos("ZJ_REGNASC")]
					SZG->ZG_CPF		:= aCols[nX,GdFieldPos("ZJ_CPF")]
					SZG->ZG_RG		:= aCols[nX,GdFieldPos("ZJ_RG")]
					SZG->ZG_DTNASC	:= aCols[nX,GdFieldPos("ZJ_DTNASC")]
					SZG->ZG_SEXO	:= aCols[nX,GdFieldPos("ZJ_SEXO")]
					SZG->ZG_NOMEMAE	:= aCols[nX,GdFieldPos("ZJ_NOMEMAE")]
					
				SZG->(MsUnlock())
			Next nX
			
			// Tabela SZJ = Integrantes
			dbSelectArea("SZJ")
			SZJ->(dbSetOrder(1))
			
			For nX := 1 To Len(aCols)
				RecLock("SZJ",.T.)
				
					SZJ->ZJ_FILIAL		:= xFilial("SZJ")
					SZJ->ZJ_FAMILIA		:= M->ZH_FAMILIA
					SZJ->ZJ_CODIGO		:= aCols[nX,GdFieldPos("ZJ_CODIGO")]
					SZJ->ZJ_DTNASC		:= aCols[nX,GdFieldPos("ZJ_DTNASC")]
					SZJ->ZJ_CODPAR		:= aCols[nX,GdFieldPos("ZJ_CODPAR")]
					SZJ->ZJ_CODPLAN		:= aCols[nX,GdFieldPos("ZJ_CODPLAN")]
					SZJ->ZJ_SOS			:= aCols[nX,GdFieldPos("ZJ_SOS")]
					SZJ->ZJ_AERO		:= aCols[nX,GdFieldPos("ZJ_AERO")]
					SZJ->ZJ_CARTEIR		:= aCols[nX,GdFieldPos("ZJ_CARTEIR")]
					SZJ->ZJ_DATAINI		:= aCols[nX,GdFieldPos("ZJ_DATAINI")]
					SZJ->ZJ_INC24H		:= aCols[nX,GdFieldPos("ZJ_INC24H")]
					SZJ->ZJ_DATATER		:= aCols[nX,GdFieldPos("ZJ_DATATER")]
					SZJ->ZJ_OBS			:= aCols[nX,GdFieldPos("ZJ_OBS")]
				
				SZJ->(MsUnlock())
			Next nX

		EndIf		
		// Grava Cabe�alho
		
		// Tabela SZH = Fam�lia
		dbSelectArea("SZH")
		SZH->(dbSetOrder(1))
		
		RecLock("SZH", .T.)
		
			SZH->ZH_FILIAL		:= xFilial("SZH")
			SZH->ZH_LOJA		:= M->ZH_LOJA
			SZH->ZH_FAMILIA		:= M->ZH_FAMILIA
			SZH->ZH_CODGRP		:= M->ZH_CODGRP
			SZH->ZH_CODTIT		:= M->ZH_CODTIT
			SZH->ZH_DTNASC		:= M->ZH_DTNASC
			SZH->ZH_CODPLAN		:= M->ZH_CODPLAN
			SZH->ZH_SOS			:= M->ZH_SOS
			SZH->ZH_AERO		:= M->ZH_AERO
			SZH->ZH_CARTEIR		:= M->ZH_CARTEIR
			SZH->ZH_DATAINI		:= M->ZH_DATAINI
			SZH->ZH_INC24H		:= M->ZH_INC24H
			SZH->ZH_DATATER		:= M->ZH_DATATER				
			SZH->ZH_OBS			:= M->ZH_OBS
		
		SZH->(MsUnlock())
	
	EndIf
	
	If nOpc == 4	// Altera��o

		// Grava �tens
		If AllTrim(aCols[1][1]) != ''		
				
			// Tabela SZG = Dependentes
			dbSelectArea("SZG")
			SZG->(dbSetOrder(1))
			
			For nX := 1 To Len(aCols)
			
				cChave := xFilial("SZG")+aCols[nX,GdFieldPos("ZJ_CODIGO")]
				
				SZG->(dbSeek(cChave))
				
				RecLock("SZG",.F.)
				
					SZG->ZG_FILIAL	:= xFilial("SZH")
					SZG->ZG_NOME	:= aCols[nX,GdFieldPos("ZJ_NOME")]
					SZG->ZG_REGNASC	:= aCols[nX,GdFieldPos("ZJ_REGNASC")]
					SZG->ZG_CPF		:= aCols[nX,GdFieldPos("ZJ_CPF")]
					SZG->ZG_RG		:= aCols[nX,GdFieldPos("ZJ_RG")]
					SZG->ZG_DTNASC	:= aCols[nX,GdFieldPos("ZJ_DTNASC")]
					SZG->ZG_SEXO	:= aCols[nX,GdFieldPos("ZJ_SEXO")]
					SZG->ZG_NOMEMAE	:= aCols[nX,GdFieldPos("ZJ_NOMEMAE")]
					
				SZG->(MsUnlock())
			Next nX
	
			// Tabela SZJ = Integrantes
			dbSelectArea("SZJ")
			SZJ->(dbSetOrder(1))
			
			// Exclui �tens anteriores
			SZJ->(dbSeek(xFilial("SZJ")+M->ZH_FAMILIA))
			
			While SZJ->(!EOF()) .And. (SZJ->ZJ_FILIAL+SZJ->ZJ_FAMILIA == xFilial("SZJ")+M->ZH_FAMILIA)
				RecLock("SZJ",.F.)
				
				SZJ->(dbDelete())
				
				SZJ->(MsUnlock())
				
				SZJ->(dbSkip())
			End
			
			// Grava Novos �tens (Alterados)
			For nX := 1 To Len(aCols)
				If !aCols[nX,Len(aHeader)+1]
					RecLock("SZJ",.T.)
					
						SZJ->ZJ_FILIAL		:= xFilial("SZJ")
						SZJ->ZJ_FAMILIA		:= M->ZH_FAMILIA
						SZJ->ZJ_CODIGO		:= aCols[nX,GdFieldPos("ZJ_CODIGO")]
						SZJ->ZJ_DTNASC		:= aCols[nX,GdFieldPos("ZJ_DTNASC")]
						SZJ->ZJ_CODPAR		:= aCols[nX,GdFieldPos("ZJ_CODPAR")]
						SZJ->ZJ_CODPLAN		:= aCols[nX,GdFieldPos("ZJ_CODPLAN")]
						SZJ->ZJ_SOS			:= aCols[nX,GdFieldPos("ZJ_SOS")]
						SZJ->ZJ_AERO		:= aCols[nX,GdFieldPos("ZJ_AERO")]
						SZJ->ZJ_CARTEIR		:= aCols[nX,GdFieldPos("ZJ_CARTEIR")]
						SZJ->ZJ_DATAINI		:= aCols[nX,GdFieldPos("ZJ_DATAINI")]
						SZJ->ZJ_INC24H		:= aCols[nX,GdFieldPos("ZJ_INC24H")]
						SZJ->ZJ_DATATER		:= aCols[nX,GdFieldPos("ZJ_DATATER")]
						SZJ->ZJ_OBS			:= aCols[nX,GdFieldPos("ZJ_OBS")]
					
					SZJ->(MsUnlock())
				EndIf
			Next nX
		EndIf
		
		// Grava Cabe�alho
		
		// Tabela SZH = Fam�lia
		dbSelectArea("SZH")
		SZH->(dbSetOrder(1))
		//SZH->(dbSeek(xFilial("SZH")+M->ZH_FAMILIA))
		
		RecLock("SZH", .F.)
		
			SZH->ZH_FILIAL		:= xFilial("SZH")
			SZH->ZH_LOJA		:= M->ZH_LOJA
			SZH->ZH_FAMILIA		:= M->ZH_FAMILIA
			SZH->ZH_CODGRP		:= M->ZH_CODGRP
			SZH->ZH_CODTIT		:= M->ZH_CODTIT
			SZH->ZH_DTNASC		:= M->ZH_DTNASC
			SZH->ZH_CODPLAN		:= M->ZH_CODPLAN
			SZH->ZH_SOS			:= M->ZH_SOS
			SZH->ZH_AERO			:= M->ZH_AERO
			SZH->ZH_CARTEIR		:= M->ZH_CARTEIR
			SZH->ZH_DATAINI		:= M->ZH_DATAINI
			SZH->ZH_INC24H		:= M->ZH_INC24H
			SZH->ZH_DATATER		:= M->ZH_DATATER				
			SZH->ZH_OBS			:= M->ZH_OBS
		
		SZH->(MsUnlock())
	EndIf
	
	If nOpc == 5	// Exclus�o
	
		Alert("N�o � poss�vel excluir Fam�lias de Planos por quest�es de hist�rico.")
		
		/*
		// Deleta os �tens
		
		// Tabela SZJ = Integrantes
		dbSelectArea("SZJ")
		SZJ->(dbSetOrder(1))
		
		SZJ->(dbSeek(xFilial("SZJ")+M->ZH_FAMILIA))
		
		While SZJ->(!EOF()) .And. (SZJ->ZJ_FILIAL+SZJ->ZJ_FAMILIA == xFilial("SZJ")+M->ZH_FAMILIA)
			RecLock("SZJ",.F.)
			
			SZJ->(dbDelete())
			
			SZJ->(MsUnlock())
			
			SZJ->(dbSkip())
		End
		
		// Deleta o Cabe�alho

		// Tabela SZH = Fam�lia
		dbSelectArea("SZH")
		SZH->(dbSetOrder(1))
		SZH->(dbSeek(xFilial("SZH")+M->ZH_FAMILIA))

		While SZH->(!EOF()) .And. (SZH->ZH_FILIAL+SZH->ZH_FAMILIA == xFilial("SZH")+M->ZH_FAMILIA)

			RecLock("SZH",.F.)
			
			SZH->(dbDelete())
			
			SZH->(MsUnlock())

			SZH->(dbSkip())
		End
		*/
	EndIf
Return


/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! LinOk		! Autor !Gilson Lima 		 ! Data ! 27/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Valida��o das linhas do Grid									 !
+----------------------------------------------------------------------------*/
User Function LinOk(nPos)
	
	Local lRet		:= .T.
	Local nX		:= 0
	Local cTitTela	:= "Manuten��o de Fam�lias"
		
	If !aCols[nPos,Len(aHeader)+1]
		
		// Valida se J� n�o existe o Dependente inserido
		For nI := 1 To Len(aCols)
			If aCols[nI,GdFieldPos("ZJ_CODIGO")] == aCols[nPos,GdFieldPos("ZJ_CODIGO")]
				nX ++
			EndIf
			If nX > 1
				MsgAlert("Dependente j� inserido nesta fam�lia!",cTitTela)
				lRet := .F.
			EndIf
		Next nI
		
		// Valida se o CPF inserido � v�lido
		If AllTrim(aCols[nPos,GdFieldPos("ZJ_CPF")]) != '' .And. !CGC(aCols[nPos,GdFieldPos("ZJ_CPF")])
			MsgAlert("CPF Informado � Inv�lido!",cTitTela)
			lRet := .F.
		EndIf
	EndIf
	
	// Limpa Vari�veis de Mem�ria para o pr�ximo registro
	M->ZJ_CODIGO	:= ''
	M->ZJ_NOME		:= ''
	M->ZJ_REGNASC	:= ''
	M->ZJ_CPF		:= ''
	M->ZJ_RG		:= ''
	M->ZJ_DTNASC	:= ''
	M->ZJ_SEXO		:= ''
	M->ZJ_NOMEMAE	:= ''
	M->ZJ_CODPAR	:= ''
	M->ZJ_DESCPAR	:= ''
	M->ZJ_CODPLAN	:= ''
	M->ZJ_DESPLAN	:= ''
	M->ZJ_CARTEIR	:= ''
	M->ZJ_DATAINI	:= StoD('')
	M->ZJ_DATATER	:= StoD('')
	M->ZJ_OBS		:= ''
	M->ZJ_IDADE		:= ''
	M->ZJ_FAIXAET	:= ''

Return(lRet)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! TudoOk	! Autor !Gilson Lima 		 ! Data ! 27/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Valida��o Final da Tela de Cadastro							 !
+----------------------------------------------------------------------------*/
Static Function TudoOk()

	Local lRet	:= .T.
	
Return(lRet)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! DelOk		! Autor !Gilson Lima 		 ! Data ! 09/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Valida��o na exclus�o do dependente							 !
+----------------------------------------------------------------------------*/
User Function DelOk(n)
	
	Local lRet	:= .F.

	// Informa que n�o � poss�vel excluir o usu�rio e confirma se deve inativ�-lo
	If MsgYesNo("Para efeito de hist�rico, n�o � poss�vel excluir um dependente. Deseja inserir a data atual no campo de t�rmino do per�odo?","Aten��o")
		If AllTrim(aCols[n,GdFieldPos("ZJ_DATATER")]) == ""
			aCols[n,GdFieldPos("ZJ_DATATER")] := dDataBase
		Else
			alert("Dependente j� possui data de t�rmino.")
		EndIf
	EndIf
	
Return(lRet)