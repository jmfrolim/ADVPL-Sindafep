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
	
	Private aCampos := {}

	// Campos a exibir da tabela de Integrantes - SZJ	
	aAdd(aCampos,"ZJ_CODPAR")
	aAdd(aCampos,"ZJ_DESCPAR")
	aAdd(aCampos,"ZJ_CODIGO")
	aAdd(aCampos,"ZJ_LOJA")
	aAdd(aCampos,"ZJ_NOME")
	aAdd(aCampos,"ZJ_CODPLAN")
	aAdd(aCampos,"ZJ_DESPLAN")
	aAdd(aCampos,"ZJ_CARTEIR")
	aAdd(aCampos,"ZJ_CODADIC")
	aAdd(aCampos,"ZJ_DESADIC")
	aAdd(aCampos,"ZJ_DTNASC")
	aAdd(aCampos,"ZJ_IDADE")
	aAdd(aCampos,"ZJ_SEXO")
	aAdd(aCampos,"ZJ_REGNASC")
	aAdd(aCampos,"ZJ_CPF")
	aAdd(aCampos,"ZJ_RG")
	aAdd(aCampos,"ZJ_NOMEMAE")
	aAdd(aCampos,"ZJ_FAIXAET")
	aAdd(aCampos,"ZJ_DATAINI")
	aAdd(aCampos,"ZJ_INC24H")
	aAdd(aCampos,"ZJ_DATATER")
	aAdd(aCampos,"ZJ_OBS")
	
	// Retorna a �rea �til das janelas Protheus
	aSize := MsAdvSize()
	
	aAdd(aObj,{100,080,.T.,.F.})
	aAdd(aObj,{100,200,.T.,.T.})
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
	
	Local cWhile 	:= "SZJ->ZJ_FILIAL+SZJ->ZJ_FAMILIA"
	Local cChave 	:= xFilial("SZJ")+SZH->ZH_FAMILIA
	Local cQuery	:= ''
		
	Private  aHeader	:= {}
	Private  aCols	:= {}
	
	Private aGets	:= {}
	Private aTela	:= {}
	
	dbSelectArea(cAlias)
	(cAlias)->(dbSetOrder(1))
	
	For nX := 1 To FCount()
		M->&(Eval(bCampo,nX)) := CriaVar(FieldName(nX),.T.)
	Next nX

	// Monta aHeader e aCols para a Grid de Integrantes
	FillGetDados(nOpc,"SZJ",5,cChave,{|| &cWhile },,,aCampos,.T.,cQuery,,.T.,@aHeader,@aCols,,,,)
	
	DEFINE MSDIALOG oDlg TITLE cCadastro FROM aSize[7],aSize[1] TO aSize[6],aSize[5] OF oMainWnd PIXEL
	
		EnChoice(cAlias, nReg, nOpc,,,,, aPObj[1])
		
		oGet := MsGetDados():New(aPObj[2,1], aPObj[2,2], aPObj[2,3], aPObj[2,4], nOpc, "U_LinOk(n)", ".T.",, .T.)
		
	ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{|| IIF(TudoOk() .And. Obrigatorio(aGets, aTela), (nOpcA := 1, oDlg:End()), NIL)},{|| oDlg:End()})
	
	If nOpcA == 1 .And. nOpc == 3
		Grava(nOpc, @aHeader, @aCols)
	EndIf

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! PSFamMan  ! Autor !Gilson Lima 		 ! Data ! 21/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao de Manuten��o do Cadastro de Fam�lias					 !
+----------------------------------------------------------------------------*/
User Function PSFamMan(cAlias,nReg,nOpc)
	Local nOpcA	:= 0
	Local cWhile 	:= "SZJ->ZJ_FILIAL+SZJ->ZJ_FAMILIA"
	Local cChave 	:= xFilial("SZJ")+SZH->ZH_FAMILIA
	Local cQuery	:= ''	
	
	Private aHeader	:= {}
	Private aCols	:= {}
	
	Private oGet
	Private oDlg
	
	Private aGets	:= {}
	Private aTela	:= {}
	Private aREG	:= {}
	
	dbSelectArea(cAlias)
	(cAlias)->(dbSetOrder(1))
	
	For nX := 1 To FCount()
		M->&(Eval(bCampo, nX)) := FieldGet(nX)
	Next nX
	
	// Monta aHeader e aCols para a Grid de Integrantes
	FillGetDados(nOpc,"SZJ",5,cChave,{|| &cWhile },,,aCampos,.T.,cQuery,,,@aHeader,@aCols,,,,)
	
	DEFINE MSDIALOG oDlg TITLE cCadastro FROM aSize[7],aSize[1] TO aSize[6],aSize[5] OF oMainWnd PIXEL
	
		EnChoice(cAlias, nReg, nOpc,,,,, aPObj[1])
		
		oGet := MsGetDados():New(aPObj[2,1], aPObj[2,2], aPObj[2,3], aPObj[2,4], nOpc, "U_LinOk(n)", ".T.",, .T.,,,,,,,,"U_DelOk(n)")
		
	ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{|| IIF(TudoOk() .And. Obrigatorio(aGets, aTela), (nOpcA := 1, oDlg:End()), NIL)},{|| oDlg:End()})

	If nOpcA == 1 .And. (nOpc == 4 .Or. nOpc == 5)
		Grava(nOpc, @aHeader, @aCols)
	EndIf	
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! Grava		! Autor !Gilson Lima 		 ! Data ! 21/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao para grava��o dos dados nas tabelas					 !
+----------------------------------------------------------------------------*/
Static Function Grava(nOpc)

	If nOpc == 3 	// Inclus�o
	
		// Grava Itens
		
		If AllTrim(aCols[1][1]) != ''		
		
			// Tabela SZG = Dependentes
			dbSelectArea("SZG")
			SZG->(dbSetOrder(1))
			
			For nX := 1 To Len(aCols)
				
				If aCols[nX,GdFieldPos("ZJ_CODPAR")] != '000000' // Diferente de Titular

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
				EndIf
			Next nX
			
			// Tabela SZJ = Integrantes
			dbSelectArea("SZJ")
			SZJ->(dbSetOrder(1))
			
			For nX := 1 To Len(aCols)
				RecLock("SZJ",.T.)
				
					SZJ->ZJ_FILIAL		:= xFilial("SZJ")
					SZJ->ZJ_FAMILIA		:= M->ZH_FAMILIA
					SZJ->ZJ_CODIGO		:= aCols[nX,GdFieldPos("ZJ_CODIGO")]
					SZJ->ZJ_NOME		:= aCols[nX,GdFieldPos("ZJ_NOME")]
					SZJ->ZJ_LOJA		:= aCols[nX,GdFieldPos("ZJ_LOJA")]
					SZJ->ZJ_DTNASC		:= aCols[nX,GdFieldPos("ZJ_DTNASC")]
					SZJ->ZJ_CODPAR		:= aCols[nX,GdFieldPos("ZJ_CODPAR")]
					SZJ->ZJ_CODPLAN		:= aCols[nX,GdFieldPos("ZJ_CODPLAN")]
					SZJ->ZJ_CODADIC		:= aCols[nX,GdFieldPos("ZJ_CODADIC")]
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
			SZH->ZH_NOMETIT		:= M->ZH_NOMETIT
			SZH->ZH_DATAINI		:= M->ZH_DATAINI
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
				
				If aCols[nX,GdFieldPos("ZJ_CODPAR")] != '000000' // Diferente de Titular
			
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
				EndIf
			Next nX

			For nX := 1 To Len(aCols)

				// Tabela SZJ = Integrantes

				//*******************************
				//* Se o Registro estiver exclu�do
				//*******************************

				If aCols[nX][Len(aCols[nX])] == .T.
					
					dbSelectArea("SZJ")
			
					// Vai para o registro correspondente
					SZJ->(dbGoTo(aCols[nX][(Len(aCols[nX]) - 1)]))
					
					RecLock("SZJ",.F.)
					
						// Exclui Registro
						SZJ->(dbDelete())				
					
					SZJ->(MsUnLock())
						
					SZJ->(dbCloseArea())		
				
				//*******************************
				//* Se for inclus�o
				//*******************************
				 	
				ElseIf aCols[nX][(Len(aCols[nX]) - 1)] == 0	
					
					dbSelectArea("SZJ")
						
						// Inclui registro
						RecLock("SZJ",.T.)
						
							SZJ->ZJ_FILIAL		:= xFilial("SZJ")
							SZJ->ZJ_FAMILIA		:= M->ZH_FAMILIA
							SZJ->ZJ_CODIGO		:= aCols[nX,GdFieldPos("ZJ_CODIGO")]
							SZJ->ZJ_NOME		:= aCols[nX,GdFieldPos("ZJ_NOME")]
							SZJ->ZJ_LOJA		:= aCols[nX,GdFieldPos("ZJ_LOJA")]
							SZJ->ZJ_DTNASC		:= aCols[nX,GdFieldPos("ZJ_DTNASC")]
							SZJ->ZJ_CODPAR		:= aCols[nX,GdFieldPos("ZJ_CODPAR")]
							SZJ->ZJ_CODPLAN		:= aCols[nX,GdFieldPos("ZJ_CODPLAN")]
							SZJ->ZJ_CODADIC		:= aCols[nX,GdFieldPos("ZJ_CODADIC")]
							SZJ->ZJ_CARTEIR		:= aCols[nX,GdFieldPos("ZJ_CARTEIR")]
							SZJ->ZJ_DATAINI		:= aCols[nX,GdFieldPos("ZJ_DATAINI")]
							SZJ->ZJ_INC24H		:= aCols[nX,GdFieldPos("ZJ_INC24H")]
							SZJ->ZJ_DATATER		:= aCols[nX,GdFieldPos("ZJ_DATATER")]
							SZJ->ZJ_OBS			:= aCols[nX,GdFieldPos("ZJ_OBS")]
						
						SZJ->(MsUnlock())
						
					SZJ->(dbCloseArea())
					
				//*******************************
				//* Se for Altera��o
				//*******************************
				
				Else
		
					dbSelectArea("SZJ")
					
					// Vai para o registro correspondente
					SZJ->(dbGoTo(aCols[nX][(Len(aCols[nX]) - 1)]))
					
						// Altera registro
						RecLock("SZJ",.F.)
						
							SZJ->ZJ_FILIAL		:= xFilial("SZJ")
							SZJ->ZJ_FAMILIA		:= M->ZH_FAMILIA
							SZJ->ZJ_CODIGO		:= aCols[nX,GdFieldPos("ZJ_CODIGO")]
							SZJ->ZJ_NOME		:= aCols[nX,GdFieldPos("ZJ_NOME")]
							SZJ->ZJ_LOJA		:= aCols[nX,GdFieldPos("ZJ_LOJA")]
							SZJ->ZJ_DTNASC		:= aCols[nX,GdFieldPos("ZJ_DTNASC")]
							SZJ->ZJ_CODPAR		:= aCols[nX,GdFieldPos("ZJ_CODPAR")]
							SZJ->ZJ_CODPLAN		:= aCols[nX,GdFieldPos("ZJ_CODPLAN")]
							SZJ->ZJ_CODADIC		:= aCols[nX,GdFieldPos("ZJ_CODADIC")]
							SZJ->ZJ_CARTEIR		:= aCols[nX,GdFieldPos("ZJ_CARTEIR")]
							SZJ->ZJ_DATAINI		:= aCols[nX,GdFieldPos("ZJ_DATAINI")]
							SZJ->ZJ_INC24H		:= aCols[nX,GdFieldPos("ZJ_INC24H")]
							SZJ->ZJ_DATATER		:= aCols[nX,GdFieldPos("ZJ_DATATER")]
							SZJ->ZJ_OBS			:= aCols[nX,GdFieldPos("ZJ_OBS")]
						
						SZJ->(MsUnlock())
						
					SZJ->(dbCloseArea())
					
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
			SZH->ZH_CODTIT		:= M->ZH_CODTIT
			SZH->ZH_NOMETIT		:= M->ZH_NOMETIT
			SZH->ZH_CODGRP		:= M->ZH_CODGRP
			SZH->ZH_DATAINI		:= M->ZH_DATAINI
			SZH->ZH_DATATER		:= M->ZH_DATATER				
			SZH->ZH_OBS			:= M->ZH_OBS
		
		SZH->(MsUnlock())
	EndIf
	
	If nOpc == 5	// Exclus�o
	
		Alert("N�o � poss�vel excluir Fam�lias de Planos por quest�es de hist�rico. Apenas desativ�-las inserindo uma data de t�rmino.")
		
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
		
		// Valida se o CPF inserido � v�lido
		If AllTrim(aCols[nPos,GdFieldPos("ZJ_CPF")]) != '' .And. !CGC(aCols[nPos,GdFieldPos("ZJ_CPF")])
			MsgAlert("CPF Informado � Inv�lido!",cTitTela)
			lRet := .F.
		EndIf
		
		// Valida se est� inserindo um plano, e caso sim, verifica 
		// se j� existe algum inserido para o integrante
		If AllTrim(aCols[nPos,GdFieldPos("ZJ_CODPLAN")]) != ''
			For nX := 1 To Len(aCols)
				If AllTrim(aCols[nX,GdFieldPos("ZJ_CODPLAN")]) != '' .And. nX != nPos;
					.And. aCols[nX,GdFieldPos("ZJ_CODIGO")] == aCols[nPos,GdFieldPos("ZJ_CODIGO")];
					.And. aCols[nX,GdFieldPos("ZJ_LOJA")] == aCols[nPos,GdFieldPos("ZJ_LOJA")];
					.And. aCols[nX,Len(aHeader)+1] != .F.
						MsgAlert("J� existe um Plano cadastrado para este Integrante.")
						lRet := .F.
				EndIf
			Next nX
		EndIf
		
		// Valida se existe Plano e Adiconal na mesma linha
		If AllTrim(aCols[nPos,GdFieldPos("ZJ_CODPLAN")]) != '' .And. AllTrim(aCols[nPos,GdFieldPos("ZJ_CODADIC")]) != ''
			MsgAlert("N�o � permitido inserir um plano e um adicional no mesmo registro.")
			lRet := .F.
		EndIf 
				
	EndIf
	
Return(lRet)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! TudoOk	! Autor !Gilson Lima 		 ! Data ! 27/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Valida��o Final da Tela de Cadastro							 !
+----------------------------------------------------------------------------*/
Static Function TudoOk()
	
	Local lRet		:= .T.
	Local lTitPlOk 	:= .F.
	Local lIntPl	:= .T.
	Local aIntegr	:= {}
	Local aIntplan	:= {}
	Local bSeek
	
	// Valida se Titular Tem plano cadastrado e ativo
	For nX := 1 To Len(aCols)
		If aCols[nX,GdFieldPos("ZJ_CODIGO")] == M->ZH_CODTIT .And. AllTrim(aCols[nX,GdFieldPos("ZJ_CODPLAN")]) != ''
			lTitPlOk := .T.
		EndIf
	Next nX
	
	If !lTitPlOk
		MsgAlert("O Titular deve ter um Plano Cadastrado")
		lRet := .F.
		Return lRet
	EndIf
	
	// Valida se todos os integrantes possuem plano cadastrado
	For nX := 1 To Len(aCols)
	
		If aCols[nX,GdFieldPos("ZJ_CODIGO")] != M->ZH_CODTIT
			bSeek := {|x| x == aCols[nX,GdFieldPos("ZJ_CODIGO")]}
			nPosIntegr := aScan(aIntegr,bSeek)
			
			If nPosIntegr == 0
				aAdd(aIntegr,aCols[nX,GdFieldPos("ZJ_CODIGO")])
			EndIf
		EndIf
	Next nX
	
	For nZ :=1 To Len(aIntegr)
		For nY := 1 To Len(aCols)
			If aCols[nY,GdFieldPos("ZJ_CODIGO")] == aIntegr[nZ] .And. AllTrim(aCols[nY,GdFieldPos("ZJ_CODPLAN")]) != ''
				aAdd(aIntPlan,{aIntegr[nZ],.T.})
			EndIf
		Next nY
		
		lIntegr := .F.
		
		For nB := 1 To Len(aIntPlan)
			If aIntPlan[nB][1] == aIntegr[nZ]
				lIntegr := .T.
			EndIf
		Next nB
		
		If !lIntegr
			aAdd(aIntPlan,{aIntegr[nZ],.F.})
		EndIf
	Next nZ
	
	For nA := 1 To Len(aIntPlan)
		If aIntPlan[nA][2] == .F.
			lIntPl := .F.
		EndIf
	Next nA
	
	If !lIntPl
		MsgAlert("� necess�rio que todos os integrantes estejam cadastrado em plano")
		lRet := .F.
		Return lRet
	EndIf
	
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