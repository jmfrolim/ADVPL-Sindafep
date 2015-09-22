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
!Nome 			   ! MANCOPAR 												 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Manutenção do arquivo de Co-participação do Plano de	 !
!				   ! Saúde Unimed 									 		 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 02/02/15 												 !
+------------------+---------------------------------------------------------+
! ATUALIZACOES 	   															 !
+-------------------------------------------+-----------+-----------+--------+
! Descricao detalhada da atualizacao 		!Nome do    ! Analista  !Data da !
! 											!Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
! 									 		! 		 	! 		 	!		 !
! 											! 		 	! 			! 		 !
+-------------------------------------------+-----------+-----------+--------+
! 											! 			! 			! 		 !
! 											! 			! 			! 		 !
+-------------------------------------------+-----------+-----------+--------*/
#include "Protheus.ch"

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! MANCOPAR	! Autor !Gilson Lima 		 ! Data ! 02/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada no menu para a manutenção da tabela			 !
! 			! de Co-participacao do Plano de Saude Unimed (SZ1)				 !
+----------------------------------------------------------------------------*/
User Function MANCOPAR()
	
	Private cFile	:= ""
	Private cPerg	:= "MANCOPAR"
	Private aPergs	:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Competência","","","mv_ch1","C",7,0,0,"G","NAOVAZIO()","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 99/9999"})
		AADD(aPergs,{"Tipo de Busca","","","mv_ch2","C",1,0,0,"C","","MV_PAR02","Nome","","","","","Família","","","","","Todos","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Buscar:","","","mv_ch3","C",50,0,0,"G","","MV_PAR03","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)
	
	// Chama a função de importação
	Processa({||MCOPAR2()},"Aguarde...")

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! MCOPAR2	! Autor !Gilson Lima 		 ! Data ! 02/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Lê a tabela de Co-participacao do Plano de Saude Unimed 		 !
! 			! trazendo os dados conforme parâmetros do usuário				 !
+----------------------------------------------------------------------------*/
Static Function MCOPAR2()

	Local cChave	:= ''
	Local cCadastro	:= 'Manutenção Co-Participação'
	Local cAlias	:= "SZ1"
	Local nRegistros:= 0

	Local oDlg
	Local oGet
	Local oTPanel1
	Local oTPanel2
	Local nOpc		:= 4
	Local aAltera 	:= {}
	
	Local aNoFields := {}
	Local cWhile 	:= "SZ1->Z1_FILIAL+SZ1->Z1_COMPFAT"

	Local aCampos 	:= {} // Campos da tabela
	Local cQuery	:= ""

	Private cCompet := MV_PAR01
	Private cCompFat:= SubStr(cCompet,4,4) + Substr(cCompet,1,2)
	
	Private nTpBusca:= MV_PAR02
	Private cBusca	:= AllTrim(MV_PAR03)
	Private cTpBusca:= ''

	Private aHeader	:= {}
	Private aCols	:= {}
	Private aREG	:= {}
	Private aRotina := {"Pesquisar"				,"AxPesqui"   ,0,1}
	
	
	cQuery	:= "SELECT * FROM " + RetSqlName("SZ1") + " SZ1"
	cQuery  += " WHERE SZ1.Z1_COMPFAT = '" + cCompFat + "'"
	cQuery	+= " AND SZ1.D_E_L_E_T_ = ' '"
	cQuery  += " AND SZ1.Z1_PROCESS = 'N'"
	
	Do Case
		Case nTpBusca == 1
			cQuery += " AND SZ1.Z1_NMBENEF LIKE '" + cBusca + "%'"
			cTpBusca := "NOME"
		Case nTpBusca == 2
			cQuery += " AND SZ1.Z1_FAMILIA LIKE '" + cBusca + "%'"
			cTpBusca := "FAMÍLIA"
		OtherWise
			cTpBusca := "TODOS"
			cBusca	 := "TODOS"
	EndCase
	
	If nTpBusca != 3
		cQuery += " ORDER BY SZ1.Z1_NMBENEF, SZ1.Z1_DTATEND, SZ1.Z1_HRATEND"
	EndIf
	
	If Select('TRB') <> 0
		dbSelectArea('TRB')
		TRB->(dbCloseArea())
	EndIf
	
	dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TRB',.F.,.F.)
	
	dbSelectArea('TRB')
	TRB->(dbGoTop())

	While TRB->(!EOF())
		nRegistros ++
		TRB->(dbSkip())	
	EndDo

	If nRegistros <= 0
		Alert('Nenhum registro encontrado com os parâmetros informados')
		Return
	EndIf
	
	// Campos a exibir da tabela	
	aAdd(aCampos,"Z1_SEQREG")
	aAdd(aCampos,"Z1_FAMILIA")
	aAdd(aCampos,"Z1_NMBENEF")
	aAdd(aCampos,"Z1_DTATEND")
	aAdd(aCampos,"Z1_HRATEND")
	aAdd(aCampos,"Z1_QTDSERV")
	aAdd(aCampos,"Z1_DESGRAU")
	aAdd(aCampos,"Z1_CODSERV")
	aAdd(aCampos,"Z1_DESPROC")
	aAdd(aCampos,"Z1_VLSERV")
	aAdd(aCampos,"Z1_NMPREST")
	aAdd(aCampos,"Z1_DEVCOB")

	// Altera
	aAdd(aAltera,"Z1_FAMILIA")
	aAdd(aAltera,"Z1_NMBENEF")
	aAdd(aAltera,"Z1_DTATEND")
	aAdd(aAltera,"Z1_HRATEND")
	aAdd(aAltera,"Z1_QTDSERV")
	aAdd(aAltera,"Z1_DESGRAU")
	aAdd(aAltera,"Z1_CODSERV")
	aAdd(aAltera,"Z1_DESPROC")
	aAdd(aAltera,"Z1_VLSERV")
	aAdd(aAltera,"Z1_NMPREST")
	aAdd(aAltera,"Z1_DEVCOB")
	
	// Monta aHeader e aCols para a Grid de Escalas
	FillGetDados(4,"SZ1",2,cChave,{|| &cWhile },,,aCampos,.T.,cQuery,,,@aHeader,@aCols,,,,)

	DEFINE MSDIALOG oDlg FROM 0,0 TO 550,1295 OF oMainWnd TITLE cCadastro PIXEL
		
		oTPanel1 		:= TPanel():New(0,0,"",oDlg,,,,,,600,50)
		
		@ 4,006 SAY "Competência: "	SIZE 70,7  PIXEL OF oTPanel1
		@ 3,050 MSGET cCompet When .F. SIZE 50,7  PIXEL OF oTPanel1
		@ 4,120 SAY "Tipo de Busca: "	SIZE 70,7  PIXEL OF oTPanel1
		@ 3,170 MSGET cTpBusca When .F. SIZE 50,7  PIXEL OF oTPanel1
		@ 4,240 SAY "Informação Buscada: "	SIZE 70,7  PIXEL OF oTPanel1
		@ 3,305 MSGET cBusca When .F. SIZE 100,7  PIXEL OF oTPanel1
		@ 4,420 SAY "Registros Encontrados: "	SIZE 70,7  PIXEL OF oTPanel1
		@ 3,485 MSGET cValtoChar(nRegistros) When .F. SIZE 50,7  PIXEL OF oTPanel1
		
		oTPanel2		:= TPanel():New(20,5,"",oDlg,,,,,,650,300)
		
		oGet := MsNewGetDados():New(0,0,240,640,GD_INSERT+GD_UPDATE+GD_DELETE,"U_SZ1LinOk(n)","U_SZ1TOK()",/*"+ZC6_ITEM"*/,aAltera,/*freeze*/,999999,/*"u_Cnt004Fld()"*/,/*superdel*/,/*"u_Cnt004Del()"*/,oTPanel2,@aHeader,@aCols) 
		
	ACTIVATE MSDIALOG oDlg CENTERED ON INIT ;
	EnchoiceBar(oDlg,{||(IIF(nOpc==4,SZ1GrvA(oGet:aHeader,oGet:aCols),;
	IIF(nOpc==5,SZ1GrvE(),oDlg:End())),oDlg:End())},;
	{||oDlg:End()})
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! SZ1LINOK	! Autor !Gilson Lima 		 ! Data ! 03/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Função para validação da Linha do Grid de Co-participação		 !
!			!																 !
+----------------------------------------------------------------------------*/
User Function SZ1LinOk(nLin)

	Local lLinOk	:= .T.
	
	// Valida Família
	cFamilia := aCols[nLin,GdFieldPos('Z1_FAMILIA')]
	
	If AllTrim(cFamilia) == ''
		lLinOk	:= .F.
		Alert('Informe a Família do Plano.')
	EndIf

	// Valida Horário Informado	
	cHrAtend := aCols[nLin,GdFieldPos('Z1_HRATEND')]
	
	If AllTrim(cHrAtend) != ''
		If Val(Substr(cHrAtend,1,2)) > 23 .Or. Val(Substr(cHrAtend,3,2)) > 59
			lLinOk	:= .F.
			Alert('A hora informada é inválida.')
		EndIf
	EndIF
	
	// Valida Tipo de Registro (Cobrança ou Devolução)
	cTpReg	:= aCols[nLin,GdFieldPos('Z1_DEVCOB')]
	
	If !cTpReg $ ("CD")
		lLinOk	:= .F.
		Alert("O tipo de registro é inválido. Informe 'C' para Cobrança ou 'D' para Devolução")
	EndIf

Return lLinOk

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! SZ1TOK	! Autor !Gilson Lima 		 ! Data ! 03/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Valida Grid antes de Gravar									 !
!			!																 !
+----------------------------------------------------------------------------*/
User Function SZ1TOk()

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! SZ1GRVA	! Autor !Gilson Lima 		 ! Data ! 03/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Função gravação de informações Incluídas, Excluídas ou		 !
!			! Alteradas no Grid de Co-participação							 !
+----------------------------------------------------------------------------*/
Static Function SZ1GrvA(aHeader, aCols)

	Local lAltera 	:= .F.
	Local nVlAnter	:= 0
	
	For nX := 1 To Len(aCols)
	
		//*******************************
		//* O Registro estiver excluído
		//*******************************
		
		If aCols[nX][Len(aCols[nX])] == .T.	

			dbSelectArea("SZ1")
			
			// Vai para o registro correspondente
			SZ1->(dbGoTo(aCols[nX][(Len(aCols[nX]) - 1)]))
			
			RecLock("SZ1",.F.)
			
				// Insere informações de quem está efetuando a exclusão
				SZ1->Z1_ALTUSER	:= CUSERNAME
				SZ1->Z1_ALTDATA	:= Date()
				SZ1->Z1_ALTHORA	:= Time()
				
				// Exclui Registro
				SZ1->(dbDelete())				
			
			SZ1->(MsUnLock())
				
			SZ1->(dbCloseArea())		
				
		//*******************************
		//* Se for inclusão
		//*******************************
		 	
		ElseIf aCols[nX][(Len(aCols[nX]) - 1)] == 0	
			
			dbSelectArea("SZ1")
				
				// Inclui registro
				RecLock("SZ1",.T.)
				
					SZ1->Z1_COMPFAT	:= cCompFat
					SZ1->Z1_FAMILIA	:= aCols[nX,GdFieldPos('Z1_FAMILIA')]
					SZ1->Z1_NMBENEF	:= aCols[nX,GdFieldPos('Z1_NMBENEF')]
					SZ1->Z1_DTATEND	:= aCols[nX,GdFieldPos('Z1_DTATEND')]
					SZ1->Z1_HRATEND	:= aCols[nX,GdFieldPos('Z1_HRATEND')]
					SZ1->Z1_QTDSERV	:= aCols[nX,GdFieldPos('Z1_QTDSERV')]
					SZ1->Z1_DESGRAU	:= aCols[nX,GdFieldPos('Z1_DESGRAU')]
					SZ1->Z1_CODSERV	:= aCols[nX,GdFieldPos('Z1_CODSERV')]
					SZ1->Z1_DESPROC	:= aCols[nX,GdFieldPos('Z1_DESPROC')]
					SZ1->Z1_VLSERV	:= aCols[nX,GdFieldPos('Z1_VLSERV')]
					SZ1->Z1_NMPREST	:= aCols[nX,GdFieldPos('Z1_NMPREST')]
					SZ1->Z1_DEVCOB	:= aCols[nX,GdFieldPos('Z1_DEVCOB')]
					SZ1->Z1_PROCESS	:= 'N'
					SZ1->Z1_ALTUSER	:= CUSERNAME
					SZ1->Z1_ALTDATA	:= Date()
					SZ1->Z1_ALTHORA	:= Time()
				
				SZ1->(MsUnLock())
				
			SZ1->(dbCloseArea())
		
		//*******************************
		//* Se for Alteração
		//*******************************
		
		Else
			
			lAltera := .F.

			dbSelectArea("SZ1")
			
			// Vai para o registro correspondente
			SZ1->(dbGoTo(aCols[nX][(Len(aCols[nX]) - 1)]))

			If SZ1->Z1_FAMILIA	!= aCols[nX,GdFieldPos('Z1_FAMILIA')]
				lAltera := .T.
			EndIf
			
			If SZ1->Z1_NMBENEF	!= aCols[nX,GdFieldPos('Z1_NMBENEF')]
				lAltera := .T.
			EndIf
			
			If SZ1->Z1_DTATEND	!= aCols[nX,GdFieldPos('Z1_DTATEND')]
				lAltera := .T.
			EndIf
			
			If SZ1->Z1_HRATEND	!= aCols[nX,GdFieldPos('Z1_HRATEND')]
				lAltera := .T.
			EndIf
			
			If SZ1->Z1_QTDSERV	!= aCols[nX,GdFieldPos('Z1_QTDSERV')]
				lAltera := .T.
			EndIf
			
			If SZ1->Z1_DESGRAU	!= aCols[nX,GdFieldPos('Z1_DESGRAU')]
				lAltera := .T.
			EndIf
			
			If SZ1->Z1_CODSERV	!= aCols[nX,GdFieldPos('Z1_CODSERV')]
				lAltera := .T.
			EndIf
			
			If SZ1->Z1_DESPROC	!= aCols[nX,GdFieldPos('Z1_DESPROC')]
				lAltera := .T.
			EndIf
			
			If SZ1->Z1_VLSERV	!= aCols[nX,GdFieldPos('Z1_VLSERV')]
				lAltera := .T.
			EndIf
			
			If SZ1->Z1_NMPREST	!= aCols[nX,GdFieldPos('Z1_NMPREST')]
				lAltera := .T.
			EndIf
			
			If SZ1->Z1_DEVCOB	!= aCols[nX,GdFieldPos('Z1_DEVCOB')]
				lAltera := .T.
			EndIf
			
			// Se houverem dados alterados
			If lAltera
			
				//Guarda Valor Anterior
				nVlAnter := SZ1->Z1_VLSERV
	
				// Se a alteração for baseada em um registro original (importado)
				If AllTrim(DtoS(SZ1->Z1_DTIMP)) != ''
				
					// Trata o registro Original
					RecLock("SZ1",.F.)
					
						// Insere informações de quem está efetuando a alteração
						SZ1->Z1_REGALTE	:= 'S'
						SZ1->Z1_ALTUSER	:= CUSERNAME
						SZ1->Z1_ALTDATA	:= Date()
						SZ1->Z1_ALTHORA	:= Time()
						
						// exclui o original
						SZ1->(dbDelete())
					
					SZ1->(MsUnLock())
						
					// Inclui novo registro com as alterações
					RecLock("SZ1",.T.)
					
						SZ1->Z1_SEQREG	:= aCols[nX,GdFieldPos('Z1_SEQREG')]
						SZ1->Z1_COMPFAT	:= cCompFat
						SZ1->Z1_FAMILIA	:= aCols[nX,GdFieldPos('Z1_FAMILIA')]
						SZ1->Z1_NMBENEF	:= aCols[nX,GdFieldPos('Z1_NMBENEF')]
						SZ1->Z1_DTATEND	:= aCols[nX,GdFieldPos('Z1_DTATEND')]
						SZ1->Z1_HRATEND	:= aCols[nX,GdFieldPos('Z1_HRATEND')]
						SZ1->Z1_QTDSERV	:= aCols[nX,GdFieldPos('Z1_QTDSERV')]
						SZ1->Z1_DESGRAU	:= aCols[nX,GdFieldPos('Z1_DESGRAU')]
						SZ1->Z1_CODSERV	:= aCols[nX,GdFieldPos('Z1_CODSERV')]
						SZ1->Z1_DESPROC	:= aCols[nX,GdFieldPos('Z1_DESPROC')]
						SZ1->Z1_VLSERV	:= aCols[nX,GdFieldPos('Z1_VLSERV')]
						SZ1->Z1_NMPREST	:= aCols[nX,GdFieldPos('Z1_NMPREST')]
						SZ1->Z1_DEVCOB	:= aCols[nX,GdFieldPos('Z1_DEVCOB')]
						SZ1->Z1_PROCESS	:= 'N'
						SZ1->Z1_REGALTE	:= 'S'
						SZ1->Z1_ALTUSER	:= CUSERNAME
						SZ1->Z1_ALTDATA	:= Date()
						SZ1->Z1_ALTHORA	:= Time()
						SZ1->Z1_VLANTER := nVlAnter
					
					SZ1->(MsUnLock())

				Else
				
					RecLock("SZ1",.F.)
					
						// Insere informações de quem está efetuando a alteração
						SZ1->Z1_REGALTE	:= 'S'
						SZ1->Z1_ALTUSER	:= CUSERNAME
						SZ1->Z1_ALTDATA	:= Date()
						SZ1->Z1_ALTHORA	:= Time()
						SZ1->Z1_VLANTER := nVlAnter
						
						// Altera os dados
						SZ1->Z1_FAMILIA	:= aCols[nX,GdFieldPos('Z1_FAMILIA')]
						SZ1->Z1_NMBENEF	:= aCols[nX,GdFieldPos('Z1_NMBENEF')]
						SZ1->Z1_DTATEND	:= aCols[nX,GdFieldPos('Z1_DTATEND')]
						SZ1->Z1_HRATEND	:= aCols[nX,GdFieldPos('Z1_HRATEND')]
						SZ1->Z1_QTDSERV	:= aCols[nX,GdFieldPos('Z1_QTDSERV')]
						SZ1->Z1_DESGRAU	:= aCols[nX,GdFieldPos('Z1_DESGRAU')]
						SZ1->Z1_CODSERV	:= aCols[nX,GdFieldPos('Z1_CODSERV')]
						SZ1->Z1_DESPROC	:= aCols[nX,GdFieldPos('Z1_DESPROC')]
						SZ1->Z1_VLSERV	:= aCols[nX,GdFieldPos('Z1_VLSERV')]
						SZ1->Z1_NMPREST	:= aCols[nX,GdFieldPos('Z1_NMPREST')]
						SZ1->Z1_DEVCOB	:= aCols[nX,GdFieldPos('Z1_DEVCOB')]			
					
					SZ1->(MsUnLock())
				EndIf
			EndIf
				
			SZ1->(dbCloseArea())
			
		EndIf
	
	Next nX
	
	MsgInfo("Manutenção efetuada com sucesso.","Atenção")

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! SZ1GRVE	! Autor !Gilson Lima 		 ! Data ! 03/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Função para validação na exclusão do Grid de Co-participação	 !
!			!																 !
+----------------------------------------------------------------------------*/
Static Function SZ1GrvE()

Return