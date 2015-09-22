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
!Nome 			   ! FIN-COTISTAS 											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para a geração de Contas a Receber das 			 !
!				   ! anuidades dos cotistas									 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 30/01/2015												 !
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
! Programa 	! GFINCOT	! Autor !Gilson Lima 		 ! Data ! 10/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Tela da Rotina Contas a Receber das Anuidade dos Cotistas		 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function GFinCot()

	Private cPerg		:= "GFINCOT"
	Private aPergs		:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Todos os Cotistas?","","","mv_ch1","C",1,0,0,"C","","MV_PAR01","Sim","","","1","","Nao","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Cotista","","","mv_ch2","C",13,0,0,"G","","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","SA1COT","","","",""})
		AADD(aPergs,{"Ano Referência","","","mv_ch3","C",4,0,0,"G","NAOVAZIO()","MV_PAR03","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Valor da Anuidade","","","mv_ch4","N",14,2,0,"G","NAOVAZIO()","MV_PAR04","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 999,999,999.99"})
		AADD(aPergs,{"Nr. de Parcelas","","","mv_ch5","N",3,0,0,"G","NAOVAZIO()","MV_PAR05","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 999"})
		AADD(aPergs,{"Data Vencto Inicial","","","mv_ch6","D",8,0,0,"G","NAOVAZIO()","MV_PAR06","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Periodicidade (dias)","","","mv_ch7","N",3,0,0,"G","NAOVAZIO()","MV_PAR07","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 999"})
		AADD(aPergs,{"Considerar Forma Cobr?","","","mv_ch8","C",1,0,0,"C","","MV_PAR08","Sim","","","1","","Nao","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Forma de Cobrança","","","mv_ch9","C",1,0,0,"C","","MV_PAR09","Financeiro","","","1","","Folha","","","","","Débito Automático","","","","","Boleto","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	If (!Pergunte(cPerg,.T.))
		Return
	EndIf

	//Chama função geração de títulos
	If MsgYesNo("Confirma a Geração dos Títulos?","Atenção")
		Processa({||GrTitCt()},"Processando Cotistas") 
	Endif 
	
Return()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GRTITCT	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para geração do Contas a Receber das anuidades dos		 !
! 			! cotistas														 !
+----------------------------------------------------------------------------*/
Static Function GrTitCt()
	

	Local 	cNrTit	 	:= ''
	Local	nCont		:= 0
	Local	nContCot	:= 0
	
	Local 	lTitOk		:= .F.
	Local   aDados		:= {}

	Local	cPrefTit	:= ''
	Local	cTipoTit	:= ''
	Local	cNatTit		:= ''
	Local	nValTit		:= 0
	Local	cHistTit	:= ''
	
	Local	aTitulo		:= {}

	Local   lProcessa	:= .F.
	
	Private lTCotistas	:= .T. 			// Todos os Cotistas (S/N) - MV_PAR01
	Private aCliente	:= StrToKArr(MV_PAR02,"|")	// Array contendo Cod Clientes e Loja
	Private cAnoRef		:= MV_PAR03		// Ano de Referência
	Private nVlAnuid	:= MV_PAR04		// Valor da Anuidade
	Private nNrParc		:= IIF(MV_PAR05 > 0, MV_PAR05, 1)		// Número de Parcelas
	Private dDtVIni		:= MV_PAR06		// Data de Vencimento Inicial
	Private nPerCob		:= MV_PAR07		// Periodicidade de Cobrança (dias)
	Private lConsFCobr	:= .F. 			// Considerar Formas de Cobrança (S/N)- MV_PAR08
	Private cFormCobr	:= cValToChar(MV_PAR09)		// Forma de Cobrança
	
	Private lMsErroAuto := .F. // Determina se houve alguma inconsistencia na execucao da rotina
	
	// Gera para todos os Cotistas: 2 = Não
	If MV_PAR01 == 2
		lTCotistas := .F.
	EndIf
	
	// Considerar Forma de Cobrança: 1 = Sim
	If MV_PAR08 == 1
		lConsFCobr := .T.
	EndIf
	
	ProcRegua(1)

	// Busca Cotistas
	dbSelectArea("SA1")
	SA1->(dbSetOrder(1))
	SA1->(dbGoTop())
	
	// Posiciona Regitro caso não sejam todos os cotistas
	If lTCotistas == .F.
		SA1->(dbSeek(xFilial("SA1")+aCliente[1]+aCliente[2]))
	EndIf
	
	// Validação para trazer apenas os cotistas ativos dentro da competência informada
	While SA1->(!EOF())
		
		lProcessa := .F.
		
		// Analisa se são todos os cotistas e efetua a validação correspondente
		If lTCotistas == .F. // Se não for todos os cotistas
			If SA1->A1_COD == aCliente[1] .And. AllTrim(SA1->A1_LOJA) == AllTrim(aCliente[2])
				
				lProcessa := .T.
			
			EndIf
		Else	// Se for todos os cotistas
			If (SA1->A1_MATPAG == cValToChar(Val(cAnoRef)-1));	// Último ano pago igual ao ano anterior ao faturamento
				.And. (SA1->A1_GRPVEN == '000002');				// Do grupo de Cotistas
				.And. (SA1->A1_SITUACA == '1')					// ATIVO | 1 = Ativo / 2 = Inativo / 3 = Transferido / 4 = Cancelado

				If lConsFCobr == .T.
					If (SA1->A1_FORMCOB == cFormCobr)			// Da Forma de Cobrança Selecionada
						lProcessa := .T.
					EndIf
				Else
					lProcessa := .T.
				EndIf
				
			EndIf
		EndIf

		If lProcessa == .T.
		
			// Zera informação de Titulo OK
			lTitOk		:= .F.
			
			// Incrementa contador de Cotistas
			nContCot++

			IncProc("Gerando Título " + cValToChar(nContCot))

			/*****************************************
			* Calcula valor a ser cobrado do cotista
			*****************************************/
			
			// Valor do Título
			nValParc := (nVlAnuid / nNrParc)
			
			/*********************************************/
			
			/*******************************************
			* Grava dados de Competência anterior e 
			* valor anterior nos campos de histórico
			* Gilson Lima - 06/08/2015
			*******************************************/
			RecLock('SA1',.F.)
			
				SA1->A1_XCPTANT := SA1->A1_MATPAG
				SA1->A1_XVLAANT := SA1->A1_VLRANUI
			
			SA1->(MsUnLock())
			/******************************************/

			// Define o nr. do título baseado na competência e tipo de cliente
			cNrTit := u_DefNrTit('01/'+cAnoRef,cValtoChar(Val(SA1->A1_GRPVEN)))
			
			cPrefTit := 'TXM'				// [PREFIXO] TXM = Taxa de Manutenção
			cNatTit	 := '1102001003'		// [NATUREZA] = Anuidade Cotistas
			
			// Define o Tipo do Título
			Do Case
				Case SA1->A1_FORMCOB == '1'
					cTipoTit	:= 'RC'
				Case SA1->A1_FORMCOB == '2'
					cTipoTit	:= 'FOL'
				Case SA1->A1_FORMCOB == '3'
					cTipoTit	:= 'DEB'
				Case SA1->A1_FORMCOB == '4'
					cTipoTit	:= 'BOL'
				Otherwise
					cTipoTit	:= 'FT'
			EndCase
			
			For nY := 1 To nNrParc

				aTitulo		:= {} 	// Zera array do título
							
				// Trata datas de vencimento
				If nY == 1
					dDtVenc := dDtVIni
				Else
					dDtVenc := dDtVenc + nPerCob
				EndIf
				dDtVencR	:= LastDay(dDtVenc,3)
				
				// Define o Histórico do Titulo
				cHistTit 	:= 'TX. MAN COL ' + PadL(cValToChar(nY),2,'0') + '/' + PadL(cValToChar(nNrParc),2,'0') + '-' + Substr(cAnoRef,3,2)
				
				// Monta array para ExecAuto
				aTitulo := {{"E1_PREFIXO",	cPrefTit,		Nil},;	// Prefixo do Título
							{"E1_NUM",		cNrTit,			Nil},;	// Número do Título
							{"E1_PARCELA",	PadL(cValToChar(nY),2,'0'), Nil},;	// Parcela do Título
							{"E1_TIPO",		cTipoTit,		Nil},;	// Tipo do Título
							{"E1_NATUREZ",	cNatTit,		Nil},;	// Natureza do Título
							{"E1_CLIENTE",	SA1->A1_COD,	Nil},;	// Código do Titular
							{"E1_LOJA",		SA1->A1_LOJA,	Nil},;	// Loja do Titular
							{"E1_EMISSAO",	dDataBase,		Nil},;	// Data de Emissão
							{"E1_VENCTO",	dDtVenc,		Nil},;	// Data de Vencimento
							{"E1_VENCREA",	dDtVencR,		Nil},;	// Data de Vencimento Real
							{"E1_VALOR",	nValParc,		Nil},;	// Valor do Título
							{"E1_MSFIL",	cFilAnt,		Nil},;	// Filial Corrente
							{"E1_HIST",		cHistTit,		Nil}}	// Histórico do Título
		
				// Executa Geração do Título se o valor for maior que 0

				If nValParc > 0
					lMsErroAuto	:= .F.
					
					MSExecAuto({|x,y| Fina040(x,y)},aTitulo,3) //3- Inclusão, 4- Alteração, 5- Exclusão
					
					If lMsErroAuto
						MostraErro()
						Return()
					EndIf
					
					// Incrementa contador de títulos gerados
					nCont++
					
					// Informa que o Titulo foi criado com sucesso
					lTitOk	:= .T.
					
				EndIf
				
				// Grava informações do faturamento na tabela de histórico se geração do título está OK
				If lTitOk
				
					aAdd(aDados,cAnoRef)
					aAdd(aDados,SA1->A1_FILIAL)
						
					GravHist(aTitulo, aDados)
				EndIf
			
			Next

			// Atualiza Valor da Anuidade
			RecLock("SA1",.F.)
	
				SA1->A1_VLRANUI := nVlAnuid			
				
			SA1->(MsUnlock())
		
		EndIf

		// Encerra processamento se não for todos os cotistas
		If lTCotistas == .F.
			Exit
		EndIf
		
		SA1->(dbSkip())
	End 
	
	SA1->(dbCloseArea())
	
	// Exibe mensagem de conclusão de geração dos títulos
	If nCont > 0
		MSGInfo(cValToChar(nContCot) + ' cotista(s) processado(s) e ' + cValToChar(nCont) + ' título(s) gerado(s) com sucesso.')
	Else
		MSGInfo("Nenhum título gerado com os parâmetros atuais.")
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
			SZL->ZL_TITPARC	:= aTitulo[3][2]
			SZL->ZL_TITFIL	:= aDados[2]		// Filial do Título
			SZL->ZL_TITTIPO	:= aTitulo[4][2]
			SZL->ZL_TITNAT	:= aTitulo[5][2]
			SZL->ZL_TITLOJA	:= aTitulo[7][2]
			SZL->ZL_TITEMIS	:= aTitulo[8][2]
			SZL->ZL_TITVNC	:= aTitulo[9][2]
			SZL->ZL_TITVNCR	:= aTitulo[10][2]
			SZL->ZL_TITVAL	:= aTitulo[11][2]
			SZL->ZL_TITCLI	:= aTitulo[6][2]
			SZL->ZL_TITHIST	:= aTitulo[12][2]
		SZL->(MsUnlock())
		SZL->(dbCloseArea())
	EndIf
	
	RestArea(aArea)
Return()
