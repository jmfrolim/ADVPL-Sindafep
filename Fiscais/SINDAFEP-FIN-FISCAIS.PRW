/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Manutenção 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Financeiro												 !
+------------------+---------------------------------------------------------+
!Nome 			   ! FIN-FISCAIS 											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para a geração de Contas a Receber das 			 !
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
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Competência","","","mv_ch1","C",7,0,0,"G","NAOVAZIO()","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 99/9999"})
		AADD(aPergs,{"Data Vencto","","","mv_ch2","D",8,0,0,"G","NAOVAZIO()","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Considerar Forma Cobr?","","","mv_ch3","C",1,0,0,"C","","MV_PAR03","Sim","","","1","","Nao","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Forma de Cobrança","","","mv_ch4","C",1,0,0,"C","","MV_PAR04","Financeiro","","","1","","Folha","","","","","Débito Automático","","","","","Boleto","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	If(!Pergunte(cPerg,.T.))
		Return
	EndIf

	//Chama função geração de títulos
	If MsgYesNo("Confirma a Geração dos Títulos?","Atenção")
		Processa({||U_GrTitFi()})
	Endif 
	
Return()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GRTITFI	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para geração do Contas a Receber das mensalidades dos	 !
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
	
	Local	nDiasFil	:= Val(GETNEWPAR("MV_DIASFIL",'15'))-1 // Nr. de Dias Filiação para gerar fatura
	Local	nDiasDesf	:= Val(GETNEWPAR("MV_DIASDSF",'5'))+1 // Nr. de Dias Desfiliação para gerar fatura
	
	Private lMsErroAuto := .F. // Determina se houve alguma inconsistencia na execucao da rotina
	
	// Define datas baseadas na Competência informada
	dPriDiaMes 	:= StoD(Substr(MV_PAR01,4,4) + Substr(MV_PAR01,1,2) + '01')
	dUltDiaMes 	:= LastDay(dPriDiaMes,0)
	cUltCFat	:= Substr(MV_PAR01,4,4) + Substr(MV_PAR01,1,2)

	// Fiscais
	dbSelectArea("SA1")
	SA1->(dbSetOrder(1))
	SA1->(dbGoTop())
	
	// Validação para trazer apenas os fiscais ativos dentro da competência informada
	While SA1->(!EOF())
		
		If (AllTrim(DtoS(SA1->A1_XDTFIL)) == '' .Or. SA1->A1_XDTFIL <= IIF(nDiasFil == 0, dUltDiaMes, DaySum(dPriDiaMes,nDiasFil)));	// Filiação anterior ao último mês da competência
			.And. (AllTrim(DtoS(SA1->A1_XDTDESF)) == '' .Or. SA1->A1_XDTDESF >= IIF(nDiasDesf == 0, dPriDiaMes, DaySum(dPriDiaMes,nDiasDesf)));// Desfiliação após o primeiro dia do mês da competência
			.And. (AllTrim(SA1->A1_XUCOMPE) == '' .Or. SA1->A1_XUCOMPE < cUltCFat);			// Última competência faturada menor que a atual
			.And. ((AllTrim(SA1->A1_XFILNSI) == '' .Or. SA1->A1_XFILNSI == 'S') .Or. (AllTrim(SA1->A1_XFILNSI) == 'N' .And. SA1->A1_XDTDESF >= dPriDiaMes));				// Filiado ao sindicato
			.And. (SA1->A1_GRPVEN == '000001' .OR. SA1->A1_GRPVEN == '000009');				// Do grupo de fiscais
			.And. ((Substr(DtoS(SA1->A1_XDTFALE),1,6) != Substr(MV_PAR01,4,4) + Substr(MV_PAR01,1,2)) .OR. AllTrim(DtoS(SA1->A1_XDTFALE)) == '' .OR. SA1->A1_XDTFALE > dUltDiaMes) // Falecimento dentro da competência não gera título
			
			If Substr(DtoS(SA1->A1_XDTFALE),1,6) == Substr(MV_PAR01,4,4) + Substr(MV_PAR01,1,2)
				SA1->(dbSkip())
				Loop
			EndIf
			
			If MV_PAR03 == 1 .And. (SA1->A1_FORMCOB != cValToChar(MV_PAR04))				// Da Forma de Cobrança Selecionada
				SA1->(dbSkip())
				Loop
			EndIf

			// Zera informação de Titulo OK
			lTitOk		:= .F.
			
			// Incrementa contador de Fiscais
			nContFis++

			/*****************************************
			* Calcula valor a ser cobrado do fiscal
			*****************************************/
			
			// Valor da Mensalidade
			nValTit := SA1->A1_VLRMENS
			
			// Atualiza valor do Título para 0,01 quando o valor estiver zerado
			If nValTit <= 0
				nValTit := 0.01
			EndIf			
			
			/*********************************************/

			aTitulo		:= {} 	// Zera array do título
					
			// Define o nr. do título baseado na competência e tipo de cliente
			cNrTit := u_DefNrTit(MV_PAR01,cValtoChar(Val(SA1->A1_GRPVEN)))
			
			//cPrefTit := 'MSL'				// [PREFIXO] MSL = Mensalidade Fiscal
			cNatTit	 := '1102001001'		// [NATUREZA] = Mensalidade Sindicato
			
			// Define o Tipo do Título
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
			
			// Define o Histórico do Titulo
			cHistTit 	:= 'MENS SINDICAL ' + Substr(MV_PAR01,1,2) + "/" + Substr(MV_PAR01,6,2)

			// Monta array para ExecAuto
			aTitulo := {{"E1_PREFIXO",	cPrefTit,		Nil},;	// Prefixo do Título
						{"E1_NUM",		cNrTit,			Nil},;	// Número do Título
						{"E1_TIPO",		cTipoTit,		Nil},;	// Tipo do Título
						{"E1_NATUREZ",	cNatTit,		Nil},;	// Natureza do Título
						{"E1_CLIENTE",	SA1->A1_COD,	Nil},;	// Código do Titular
						{"E1_LOJA",		SA1->A1_LOJA,	Nil},;	// Loja do Titular
						{"E1_EMISSAO",	dDataBase,		Nil},;	// Data de Emissão
						{"E1_VENCTO",	dDtVenc,		Nil},;	// Data de Vencimento
						{"E1_VENCREA",	dDtVencR,		Nil},;	// Data de Vencimento Real
						{"E1_VALOR",	nValTit,		Nil},;	// Valor do Título
						{"E1_MSFIL",	cFilAnt,		Nil},;	// Filial do Titulo
						{"E1_HIST",		cHistTit,		Nil}}	// Histórico do Título
	
			// Executa Geração do Título se o valor for maior que 0
			
			If nValTit > 0
				lMsErroAuto	:= .F.
				
				MSExecAuto({|x,y| Fina040(x,y)},aTitulo,3) //3- Inclusão, 4- Alteração, 5- Exclusão
				
				If lMsErroAuto
					MostraErro()
					Return()
				EndIf
				
				// Incrementa contador de títulos gerados
				nCont++
				
				IncProc("Gerado Título " + cValToChar(nCont))
				
				// Informa que o Titulo foi criado com sucesso
				lTitOk	:= .T.
			EndIf
	
			// Atribui competência de faturamento no cadastro de famílias
			RecLock("SA1",.F.)
	
				SA1->A1_XUCOMPE := cUltCFat			
				
			SA1->(MsUnlock())
			
			// Grava informações do faturamento na tabela de histórico se geração do título está OK
			If lTitOk
			
				aAdd(aDados,cUltCFat)
				aAdd(aDados,SA1->A1_FILIAL)
					
				GravHist(aTitulo, aDados)
			EndIf
		EndIf
		
		SA1->(dbSkip())
	End 
	
	SA1->(dbCloseArea())
	
	// Exibe mensagem de conclusão de geração dos títulos
	If nCont > 0
		MSGInfo(cValToChar(nContFis) + ' fiscal(is) processado(s) e ' + cValToChar(nCont) + ' título(s) gerado(s) com sucesso.')
	Else
		MSGInfo("Nenhum título a ser gerado com os parâmetros atuais.")
	EndIf
	
Return ()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GRAVHIST	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para gravação de histórico dos titulos gerados para a	 !
! 			! mensalidade dos fiscais														 !
+----------------------------------------------------------------------------*/
Static Function GravHist(aTitulo, aDados)
	
	Local aArea := GetArea()
	
	If Len(aTitulo) > 0

		// Grava Informações do Título
		dbSelectArea("SZL")
		RecLock('SZL', .T.)
			
			SZL->ZL_FILIAL	:= xFilial('SZL')
			SZL->ZL_TPDADO	:= 'F'				// Financeiro
			SZL->ZL_COMPET	:= aDados[1]		// Competência
			SZL->ZL_TITPREF	:= aTitulo[1][2]
			SZL->ZL_TITNUM	:= aTitulo[2][2]
			SZL->ZL_TITFIL	:= aDados[2]		// Filial do Título
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
