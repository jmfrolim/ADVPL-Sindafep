/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Manuten��o 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Financeiro												 !
+------------------+---------------------------------------------------------+
!Nome 			   ! FIN-FISCAIS 											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para a gera��o de Contas a Receber das 			 !
!				   ! mensalidades dos fiscais								 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 23/12/2014												 !
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
! Programa 	! GFINFIS	! Autor !Gilson Lima 		 ! Data ! 10/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Tela da Rotina Contas a Receber das Mensalidades dos Fiscais	 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function GFinFis()

	Private cPerg		:= "GFINFIS"
	Private aPergs		:= {}
	
	// Cria pergunta caso n�o exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Compet�ncia","","","mv_ch1","C",7,0,0,"G","NAOVAZIO()","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 99/9999"})
		AADD(aPergs,{"Data Vencto","","","mv_ch2","D",8,0,0,"G","NAOVAZIO()","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Considerar Forma Cobr?","","","mv_ch3","C",1,0,0,"C","","MV_PAR03","Sim","","","1","","Nao","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Forma de Cobran�a","","","mv_ch4","C",1,0,0,"C","","MV_PAR04","Financeiro","","","1","","Folha","","","","","D�bito Autom�tico","","","","","Boleto","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	If(!Pergunte(cPerg,.T.))
		Return
	EndIf

	//Chama fun��o gera��o de t�tulos
	If MsgYesNo("Confirma a Gera��o dos T�tulos?","Aten��o")
		Processa({||U_GrTitFi()})
	Endif 
	
Return()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GRTITFI	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para gera��o do Contas a Receber das mensalidades dos	 !
! 			! fiscais														 !
+----------------------------------------------------------------------------*/
User Function GrTitFi()
	
	Local 	cNrTit	 	:= ''
	Local	cCliTipo	:= ''
	Local	cUltCFat	:= ''
	Local	nCont		:= 0
	Local	nContFis	:= 0
	
	Local 	lTitOk		:= .F.
	Local   aDados		:= {}
	
	Local	aIntegrant	:= {}
	Local	aIntegr		:= {}

	Local	cPrefTit	:= ''
	Local	cTipoTit	:= ''
	Local	cNatTit		:= ''
	Local	nValTit		:= 0
	Local	cHistTit	:= ''
	
	Local	aTitulo		:= {}

	Local	dPriDiaMes 	:= ''
	Local	dUltDiaMes	:= ''
	
	Local	nDiasFil	:= Val(GETNEWPAR("MV_DIASFIL",'15'))-1 // Nr. de Dias Filia��o para gerar fatura
	Local	nDiasDesf	:= Val(GETNEWPAR("MV_DIASDSF",'5'))+1 // Nr. de Dias Desfilia��o para gerar fatura
	
	Private lMsErroAuto := .F. // Determina se houve alguma inconsistencia na execucao da rotina
	
	// Define datas baseadas na Compet�ncia informada
	dPriDiaMes 	:= StoD(Substr(MV_PAR01,4,4) + Substr(MV_PAR01,1,2) + '01')
	dUltDiaMes 	:= LastDay(dPriDiaMes,0)
	cUltCFat	:= Substr(MV_PAR01,4,4) + Substr(MV_PAR01,1,2)

	// Fiscais
	dbSelectArea("SA1")
	SA1->(dbSetOrder(1))
	SA1->(dbGoTop())
	
	// Valida��o para trazer apenas os fiscais ativos dentro da compet�ncia informada
	While SA1->(!EOF())
		
		If (AllTrim(DtoS(SA1->A1_XDTFIL)) == '' .Or. SA1->A1_XDTFIL <= IIF(nDiasFil == 0, dUltDiaMes, DaySum(dPriDiaMes,nDiasFil)));	// Filia��o anterior ao �ltimo m�s da compet�ncia
			.And. (AllTrim(DtoS(SA1->A1_XDTDESF)) == '' .Or. SA1->A1_XDTDESF >= IIF(nDiasDesf == 0, dPriDiaMes, DaySum(dPriDiaMes,nDiasDesf)));// Desfilia��o ap�s o primeiro dia do m�s da compet�ncia
			.And. (AllTrim(SA1->A1_XUCOMPE) == '' .Or. SA1->A1_XUCOMPE < cUltCFat);			// �ltima compet�ncia faturada menor que a atual
			.And. ((AllTrim(SA1->A1_XFILNSI) == '' .Or. SA1->A1_XFILNSI == 'S') .Or. (AllTrim(SA1->A1_XFILNSI) == 'N' .And. SA1->A1_XDTDESF >= dPriDiaMes));				// Filiado ao sindicato
			.And. (SA1->A1_GRPVEN == '000001' .OR. SA1->A1_GRPVEN == '000009');				// Do grupo de fiscais
			.And. ((Substr(DtoS(SA1->A1_XDTFALE),1,6) != Substr(MV_PAR01,4,4) + Substr(MV_PAR01,1,2)) .OR. AllTrim(DtoS(SA1->A1_XDTFALE)) == '' .OR. SA1->A1_XDTFALE > dUltDiaMes) // Falecimento dentro da compet�ncia n�o gera t�tulo
			
			If Substr(DtoS(SA1->A1_XDTFALE),1,6) == Substr(MV_PAR01,4,4) + Substr(MV_PAR01,1,2)
				SA1->(dbSkip())
				Loop
			EndIf
			
			If MV_PAR03 == 1 .And. (SA1->A1_FORMCOB != cValToChar(MV_PAR04))				// Da Forma de Cobran�a Selecionada
				SA1->(dbSkip())
				Loop
			EndIf

			// Zera informa��o de Titulo OK
			lTitOk		:= .F.
			
			// Incrementa contador de Fiscais
			nContFis++

			/*****************************************
			* Calcula valor a ser cobrado do fiscal
			*****************************************/
			
			// Valor da Mensalidade
			nValTit := SA1->A1_VLRMENS
			
			// Atualiza valor do T�tulo para 0,01 quando o valor estiver zerado
			If nValTit <= 0
				nValTit := 0.01
			EndIf			
			
			/*********************************************/

			aTitulo		:= {} 	// Zera array do t�tulo
					
			// Define o nr. do t�tulo baseado na compet�ncia e tipo de cliente
			cNrTit := u_DefNrTit(MV_PAR01,cValtoChar(Val(SA1->A1_GRPVEN)))
			
			//cPrefTit := 'MSL'				// [PREFIXO] MSL = Mensalidade Fiscal
			cNatTit	 := '1102001001'		// [NATUREZA] = Mensalidade Sindicato
			
			// Define o Tipo do T�tulo
			Do Case
				Case SA1->A1_FORMCOB == '1'
					cTipoTit	:= 'RC'
					cPrefTit	:= 'MSL'
				Case SA1->A1_FORMCOB == '2'
					cTipoTit	:= '5MH'
					cPrefTit	:= 'MS1'
				Case SA1->A1_FORMCOB == '3'
					cTipoTit	:= 'DEB'
					cPrefTit	:= 'MS2'
				Case SA1->A1_FORMCOB == '4'
					cTipoTit	:= 'BOL'
					cPrefTit	:= 'MS3'
				OtherWise
					cTipoTit	:= 'RC'
					cPrefTit	:= 'MSL'
			EndCase
						
			// Trata datas de vencimento
			dDtVenc		:= IIF (AllTrim(DtoS(MV_PAR02)) != '', MV_PAR02, dDataBase)	
			dDtVencR	:= LastDay(dDtVenc,3)
			
			// Define o Hist�rico do Titulo
			cHistTit 	:= 'MENS SINDICAL ' + Substr(MV_PAR01,1,2) + "/" + Substr(MV_PAR01,6,2)

			// Monta array para ExecAuto
			aTitulo := {{"E1_PREFIXO",	cPrefTit,		Nil},;	// Prefixo do T�tulo
						{"E1_NUM",		cNrTit,			Nil},;	// N�mero do T�tulo
						{"E1_TIPO",		cTipoTit,		Nil},;	// Tipo do T�tulo
						{"E1_NATUREZ",	cNatTit,		Nil},;	// Natureza do T�tulo
						{"E1_CLIENTE",	SA1->A1_COD,	Nil},;	// C�digo do Titular
						{"E1_LOJA",		SA1->A1_LOJA,	Nil},;	// Loja do Titular
						{"E1_EMISSAO",	dDataBase,		Nil},;	// Data de Emiss�o
						{"E1_VENCTO",	dDtVenc,		Nil},;	// Data de Vencimento
						{"E1_VENCREA",	dDtVencR,		Nil},;	// Data de Vencimento Real
						{"E1_VALOR",	nValTit,		Nil},;	// Valor do T�tulo
						{"E1_MSFIL",	cFilAnt,		Nil},;	// Filial do Titulo
						{"E1_HIST",		cHistTit,		Nil}}	// Hist�rico do T�tulo
	
			// Executa Gera��o do T�tulo se o valor for maior que 0
			
			If nValTit > 0
				lMsErroAuto	:= .F.
				
				MSExecAuto({|x,y| Fina040(x,y)},aTitulo,3) //3- Inclus�o, 4- Altera��o, 5- Exclus�o
				
				If lMsErroAuto
					MostraErro()
					Return()
				EndIf
				
				// Incrementa contador de t�tulos gerados
				nCont++
				
				IncProc("Gerado T�tulo " + cValToChar(nCont))
				
				// Informa que o Titulo foi criado com sucesso
				lTitOk	:= .T.
			EndIf
	
			// Atribui compet�ncia de faturamento no cadastro de fam�lias
			RecLock("SA1",.F.)
	
				SA1->A1_XUCOMPE := cUltCFat			
				
			SA1->(MsUnlock())
			
			// Grava informa��es do faturamento na tabela de hist�rico se gera��o do t�tulo est� OK
			If lTitOk
			
				aAdd(aDados,cUltCFat)
				aAdd(aDados,SA1->A1_FILIAL)
					
				GravHist(aTitulo, aDados)
			EndIf
		EndIf
		
		SA1->(dbSkip())
	End 
	
	SA1->(dbCloseArea())
	
	// Exibe mensagem de conclus�o de gera��o dos t�tulos
	If nCont > 0
		MSGInfo(cValToChar(nContFis) + ' fiscal(is) processado(s) e ' + cValToChar(nCont) + ' t�tulo(s) gerado(s) com sucesso.')
	Else
		MSGInfo("Nenhum t�tulo a ser gerado com os par�metros atuais.")
	EndIf
	
Return ()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GRAVHIST	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para grava��o de hist�rico dos titulos gerados para a	 !
! 			! mensalidade dos fiscais														 !
+----------------------------------------------------------------------------*/
Static Function GravHist(aTitulo, aDados)
	
	Local aArea := GetArea()
	
	If Len(aTitulo) > 0

		// Grava Informa��es do T�tulo
		dbSelectArea("SZL")
		RecLock('SZL', .T.)
			
			SZL->ZL_FILIAL	:= xFilial('SZL')
			SZL->ZL_TPDADO	:= 'F'				// Financeiro
			SZL->ZL_COMPET	:= aDados[1]		// Compet�ncia
			SZL->ZL_TITPREF	:= aTitulo[1][2]
			SZL->ZL_TITNUM	:= aTitulo[2][2]
			SZL->ZL_TITFIL	:= aDados[2]		// Filial do T�tulo
			SZL->ZL_TITTIPO	:= aTitulo[3][2]
			SZL->ZL_TITNAT	:= aTitulo[4][2]
			SZL->ZL_TITLOJA	:= aTitulo[6][2]
			SZL->ZL_TITEMIS	:= aTitulo[7][2]
			SZL->ZL_TITVNC	:= aTitulo[8][2]
			SZL->ZL_TITVNCR	:= aTitulo[9][2]
			SZL->ZL_TITVAL	:= aTitulo[10][2]
			SZL->ZL_TITCLI	:= aTitulo[5][2]
			SZL->ZL_TITHIST	:= aTitulo[11][2]
		SZL->(MsUnlock())
		SZL->(dbCloseArea())
	EndIf
	
	RestArea(aArea)
Return()
