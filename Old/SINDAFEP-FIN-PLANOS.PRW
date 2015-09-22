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
!Nome 			   ! FIN-PLANOS 											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para a geração de Contas a Receber dos Planos	 !
!				   ! de Saúde o Odontológicos								 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 10/12/2014												 !
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
! Programa 	! GFINPLS	! Autor !Gilson Lima 		 ! Data ! 10/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para geração do Contas a Receber dos Planos de Saúde	 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function GFinPls()
	
	Local 	cNrTit	 	:= ''
	Local	cCliTipo	:= ''
	Local	cUltCFat	:= ''
	Local	nCont		:= 0
	Local	aCoPart 	:= {}
	Local	aPlanoDep	:= {}

	Local	nTotCoPart	:= 0
	Local	nTotTit		:= 0
	Local	nTotDep		:= 0
	Local	nVlDep		:= 0
		
	Local	cPrefTit	:= ''
	Local	cTipoTit	:= ''
	Local	cNatTit		:= ''
	Local	nValTit		:= 0
	Local	cHistTit	:= ''
	
	Local	aTitulo		:= {}
	Local	aTitulos	:= {}

	Local	dPriDiaMes 	:= ''
	Local	dUltDiaMes	:= ''
	
	Private cPerg	:= "GFINPLS"
	Private aPergs	:= {}
	
	Private lMsErroAuto := .F. // Determina se houve alguma inconsistencia na execucao da rotina
	
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Competência","","","mv_ch1","C",7,0,0,"G","","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 99/9999"})
		AADD(aPergs,{"Todos os Clientes?","","","mv_ch2","C",1,0,0,"C","","MV_PAR02","S=Sim","","","S","","N=Nao","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Grupo de Clientes","","","mv_ch3","C",6,0,0,"G","","MV_PAR03","","","","","","","","","","","","","","","","","","","","","","","","","ACY","","","",""})
		AADD(aPergs,{"Data Vencto","","","mv_ch4","D",8,0,0,"G","","MV_PAR04","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)
	
	// Define datas baseadas na Competência informada
	dPriDiaMes 	:= StoD(Substr(MV_PAR01,4,4) + Substr(MV_PAR01,1,2) + '01')
	dUltDiaMes 	:= LastDay(dPriDiaMes,0)
	cUltCFat	:= Substr(MV_PAR01,4,4) + Substr(MV_PAR01,1,2)
	
	// Busca Famílias
	dbSelectArea("SZH")
	SZH->(dbSetOrder(1))
	SZH->(dbGoTop())
	
	// Validação para trazer apenas as famílias ativas dentro da competência informada
	While SZH->(!EOF())  .And. (AllTrim(SZH->ZH_DATAINI) == '' .Or. SZH->ZH_DATAINI <= dUltDiaMes);
						 .And. (AllTrim(SZH->ZH_DATATER) == '' .Or. SZH->ZH_DATATER >= dPriDiaMes);
						 .And. (AllTrim(SZH->ZH_ULTCFAT) == '' .Or. SZH->ZH_ULTCFAT < cUltCFat)
		
		// Busca tipo do cliente
		cCliTipo	:= POSICIONE("SA1",1,xFilial("SA1")+SZH->ZH_CODTIT,"A1_GRPVEN")
		
		If (MV_PAR02 != 1) .And. (MV_PAR03 != cCliTipo) // Se não for para faturar todas as famílias,
														// e o tipo do cliente não for o tipo escolhido, vai para a próxima família
			Loop
		EndIf
		
		// Retorna array com informações do plano do titular
		aPlanoTit	:= u_CalcPlano(SZH->ZH_CODPLAN,SZH->ZH_DTNASC,MV_PAR01)
		
		/*****************************************
		* Calcula valor a ser cobrado do titular
		*****************************************/
		
		// Valor do Plano
		nTotTit	:= aPlanoTit[7]		// Mensalidade

		// Se for funcionário, calcula taxa
		If cCliTipo == '000003'	// 000003 = Funcionário
			nTotTit := nTotTit * (aPlanoTit[13] / 100)
		EndIf

		nTotTit += aPlanoTit[10]	// Fundo
		nTotTit += aPlanoTit[11]	// Taxa Administrativa
		
		// Valor do SOS
		If SZH->ZH_SOS == 'S'
			nTotTit += aPlanoTit[8]
		EndIf
		
		// Valor do Aero
		If SZH->ZH_AERO == 'S'
			nTotTit	+= aPlanoTit[9]
		EndIf
		
		/*********************************************/

		aTPlanDep	:= {} 	// Zera array dos planos dos dependentes
		aTitulo		:= {} 	// Zera array do título
		lMsErroAuto	:= .F.
				
		// Busca dependentes da família
		dbSelectArea("SZJ")
		SZJ->(dbSetOrder(1))
		SZJ->(dbGoTop())
		
		SZJ->(dbSeek(xFilial('SZJ')+SZH->ZH_FAMILIA))
		
		// Busca dependentes ativos dentro da competência informada
		While SZJ->(!EOF()) .And. (AllTrim(SZJ->ZJ_DATAINI) == '' .Or. SZJ->ZJ_DATAINI <= dUltDiaMes);
						    .And. (AllTrim(SZJ->ZJ_DATATER) == '' .Or. SZJ->ZJ_DATATER >= dPriDiaMes);
						    .And. SZJ->ZJ_FAMILIA == SZH->ZH_FAMILIA
			
			aPlanoDep := u_CalcPlano(SZJ->ZJ_CODPLAN,SZJ->ZJ_DTNASC,MV_PAR01)
			
			/*****************************************
			* Calcula valor a ser cobrado do dependente
			*****************************************/
			
			// Valor do Plano
			nVlDep := aPlanoDep[7]	// Mensalidade

			// Se for funcionário, calcula taxa
			If cCliTipo == '000003'	// 000003 = Funcionário
				nVlDep := nVlDep * (aPlanoDep[13] / 100)
			EndIf

			nVlDep += aPlanoDep[10]	// Fundo
			nVlDep += aPlanoDep[11]	// Taxa Administrativa
			
			// Valor do SOS
			If SZJ->ZJ_SOS == 'S'
				nVlDep += aPlanoDep[8]
			EndIf
			
			// Valor do Aero
			If SZJ->ZJ_AERO == 'S'
				nVlDep	+= aPlanoDep[9]
			EndIf
			
			// Adiciona o valor do Depentende ao total dos Dependentes
			nTotDep += nVlDep
	
			/*********************************************/			
			   
			SZJ->(dbSkip())			    
		End
		
		SZJ->(dbCloseArea())
		
		// Busca informações da Coparticipação da família
		aCoPart := u_CalcCoPar(Substr(SZH->ZH_FAMILIA,1,7),MV_PAR01)
		
		// Totaliza Coparticipação da família
		For Nx := 1 To Len(aCoPart)
			nCont ++
			If aCopart[nX][10] == 'C'
				nTotCoPart += aCoPart[nX][9]
			Else
				nTotCoPart -= aCopart[nX][9]
			EndIf
		Next Nx	
		
		// Define o nr. do título baseado na competência e tipo de cliente
		cNrTit:= u_DefNrTit(MV_PAR01,cValtoChar(Val(cCliTipo)))
		
		// Define o Prefixo e Natureza do Título
		If SZH->ZH_CODGRP == '000001' 		// 000001 = Unimed
			
			cPrefTit := 'PLS'				// [PREFIXO] PLS = Planos de Saúde
			cNatTit	 := '1102001006'		// [NATUREZA] = Mensalidade Saúde
		
		ElseIf SZH->ZH_CODGRP == '000002'	// 000002 = Odontouni
			
			cPrefTit := 'PLO'				// [PREFIXO] PLO = Planos Odontológicos
			cNatTit	 := '1102001007'		// [NATUREZA] = Mensalidade Odonto
		
		EndIf
		
		// Define o Tipo do Título
		cTipoTit := 'FT'					// FT = Fatura
		
		// Trata datas de vencimento
		dDtVenc		:= IIF (AllTrim(DtoS(MV_PAR04)) != '', MV_PAR04, dDataBase)	
		dDtVencR	:= LastDay(dDtVenc,3)
		
		// Define o Histórico do Titulo
		cDescrGrp	:= POSICIONE('SZD',1,xFilial('SZD')+SZH->ZH_CODGRP,'ZD_DESCR')
		cHistTit := AllTrim(cDescrGrp) + ' - ' + MV_PAR01
		
		// Calcula o valor total do titulo
		nValTit := nTotTit + nTotDep + nTotCoPart
		
		// Monta array para ExecAuto
		/*
		aAdd(aTitulo,{"E1_PREFIXO",	cPrefTit,		Nil})	// Prefixo do Título
		aAdd(aTitulo,{"E1_NUM",		cNrTit,			Nil})	// Número do Título
		aAdd(aTitulo,{"E1_TIPO",	cTipoTit,		Nil})	// Tipo do Título
		aAdd(aTitulo,{"E1_NATUREZ",	cNatTit,		Nil})	// Natureza do Título
		aAdd(aTitulo,{"E1_CLIENTE",	SZH->ZH_CODTIT,	Nil})	// Código do Titular
		aAdd(aTitulo,{"E1_EMISSAO",	dDataBase,		Nil})	// Data de Emissão
		aAdd(aTitulo,{"E1_VENCTO",	dDtVenc,		Nil})	// Data de Vencimento
		aAdd(aTitulo,{"E1_VENCREA",	dDtVencR,		Nil})	// Data de Vencimento Real
		aAdd(aTitulo,{"E1_VALOR",	nValTit,		Nil})	// Valor do Título
		aAdd(aTitulo,{"E1_HIST",	cHistTit,		Nil})	// Histórico do Título
		*/

		// Monta array para ExecAuto
		aTitulo := {{"E1_PREFIXO",	cPrefTit,		Nil},;	// Prefixo do Título
					{"E1_NUM",		cNrTit,			Nil},;	// Número do Título
					{"E1_TIPO",		cTipoTit,		Nil},;	// Tipo do Título
					{"E1_NATUREZ",	cNatTit,		Nil},;	// Natureza do Título
					{"E1_CLIENTE",	SZH->ZH_CODTIT,	Nil},;	// Código do Titular
					{"E1_EMISSAO",	dDataBase,		Nil},;	// Data de Emissão
					{"E1_VENCTO",	dDtVenc,		Nil},;	// Data de Vencimento
					{"E1_VENCREA",	dDtVencR,		Nil},;	// Data de Vencimento Real
					{"E1_VALOR",	nValTit,		Nil},;	// Valor do Título
					{"E1_HIST",		cHistTit,		Nil}}	// Histórico do Título

		// Executa Geração do Título
		MSExecAuto({|x,y| Fina040(x,y)},aTitulo,3) //3- Inclusão, 4- Alteração, 5- Exclusão
		
		If lMsErroAuto
			MostraErro()
			Return()
		Else
			// Atribui competência de faturamento no cadastro de famílias
			RecLock("SZH",.F.)

				SZH->ZH_ULTCFAT := cUltCFat			
				
			SZH->(MsUnlock())
			
			// Marca Flag como Processada na tabela de CoParticipação
			// baseado no Array retornado pela rotina de CoParticipação
			For Nx := 1 To Len(aCoPart)
				
				dbSelectArea("SZ1")
				SZ1->(dbGoTo(aCoPart[Nx][1]))
				
				RecLock("SZ1",.F.)
				
					SZ1->Z1_PROCESS := 'S'
				
				SZ1->(MsUnlock())
			Next Nx
			
			// Grava informações do faturamento na tabela de histórico
			
		EndIf		
			
		SZH->(dbSkip())
	End 
	
Return ()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! DefNrTit	! Autor !Gilson Lima 		 ! Data ! 12/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao para geração do número do Título						 !
+----------------------------------------------------------------------------*/
User Function DefNrTit(cCompet,cTipo)

	Local cAnoCompet := Substr(cCompet,6,2)
	Local cMesCompet := Substr(cCompet,1,2)
	Local cNumero	 := ''
	Local cQuery	 := '' 
	
	cQuery	:= "SELECT SE1.E1_NUM FROM " + RetSqlName('SE1') + " SE1"
	cQuery	+= " WHERE SE1.D_E_L_E_T_ = ' '"
	cQuery	+= " AND SE1.E1_NUM Like '" + cAnoCompet + cMesCompet + cTipo +"%'"
	cQuery 	+= " ORDER BY SE1.E1_NUM DESC"
	
	If Select('SE1') <> 0
		dbSelectArea('SE1')
		dbCloseArea()
	EndIf
	
	dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'SE1',.F.,.F.)
	
	dbSelectArea('SE1')
	SE1->(dbGoTop())
	
	While SE1->(!EOF())
		cNumero := SE1->E1_NUM
		Exit
	End
	
	If AllTrim(cNumero) == ''
		cTitulo := cAnoCompet+cMesCompet+cTipo+"0001"
	Else
		cTitulo := soma1(cNumero)
	EndIf
	
Return (cTitulo)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! CalcIdade	! Autor !Gilson Lima 		 ! Data ! 26/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao cálculo da idade baseada na Dt. de Nascimento			 !
+----------------------------------------------------------------------------*/
User Function CalcIdade(dDtNasc)

	Local cIdade		:= ''
	Local nIdade		:= U_ClIdade(dDtNasc, dDataBase)
	
	cIdade := cValToChar(nIdade) + " anos"
	
Return(cIdade)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! ClIdade	! Autor !Gilson Lima 		 ! Data ! 26/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao cálculo da idade baseada na Dt. de Nascimento			 !
+----------------------------------------------------------------------------*/
User Function ClIdade(dDtNasc, dDtCalc)
	
	Local cMesDiaNasc	:= ''
	Local cMesDiaBase	:= ''
	Local cIdade		:= ''
	Local nIdade		:= 0
	
	cMesDiaNasc	:= StrZero(Month(dDtNasc),2) + StrZero(Day(dDtNasc),2)
	cMesDiaBase	:= StrZero(Month(dDtCalc),2) + StrZero(Day(dDtCalc),2)
	nIdade		:= Year(dDtCalc) - Year(dDtNasc)
	
	If (cMesDiaNasc > cMesDiaBase)
		nIdade --
	EndIf
	
Return(nIdade)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! CalcFaixa	! Autor !Gilson Lima 		 ! Data ! 26/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao cálculo da idade baseada na Dt. de Nascimento e Plano	 !
+----------------------------------------------------------------------------*/
User Function CalcFaixa(cCodPlan,dDtNasc)

	Local aPlano := {}
	Local cCompet	:= Substr(DtoS(dDataBase),5,2)+'/'+Substr(DtoS(dDataBase),1,4)
	
	aPlano := U_CalcPlano(cCodPlan,dDtNasc,cCompet)

Return(aPlano[2])

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! ClFaixaPl	! Autor !Gilson Lima 		 ! Data ! 26/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao cálculo da idade baseada na Dt. de Nascimento e Plano	 !
+----------------------------------------------------------------------------*/
User Function CalcPlano(cCodPlan,dDtNasc,cCompet)

	Local aArea		:= GetArea()
	Local cAnoNasc	:= ''
	Local cMesNasc	:= ''
	Local cDiaNasc	:= ''
	Local cMesBase	:= ''
	Local cDiaBase	:= ''
	Local cIdade	:= ''
	Local nIdade	:= 0
	
	Local cGrpPlan	:= ''
	Local cGrpDesc	:= ''
	Local cCodDFx	:= ''
	Local cDescPlan	:= ''
	Local nCobPlan	:= 0
	Local cCodFx	:= ''
	Local nValor	:= 0
	Local nVlSOS	:= 0
	Local nVlAero	:= 0
	Local nFundo	:= 0
	Local nTxAdm	:= 0
	Local nPerInss	:= 0
	Local nPerFunc	:= 0	
	
	Local aPlano	:= {}
	
	Local cQuery	:= ''
	Local cDescrFx	:= ''
	Local nLimIdade	:= 0
	Local dDtCalc	:= StoD(Substr(cCompet,4,4) + Substr(cCompet,1,2) + '01')
	
	cAnoNasc	:= StrZero(Year(dDtNasc),4)
	
	cMesNasc	:= StrZero(Month(dDtNasc),2)
	cMesBase	:= Substr(cCompet,1,2)
	
	// Calcula a Idade
	If cMesNasc == cMesBase
		nIdade		:= U_ClIdade(StoD(cAnoNasc+cMesNasc+'01'),dDtCalc)
	Else
		nIdade		:= U_ClIdade(dDtNasc,dDtCalc)
	EndIf
	
	// Busca Grupo do Plano
	cGrpPlan	:= Posicione("SZE",1,xFilial("SZE")+cCodPlan,"ZE_CODGRP")
	cDescPlan	:= Posicione("SZE",1,xFilial("SZE")+cCodPlan,"ZE_DESCR")
	nCobPlan	:= Posicione("SZE",1,xFilial("SZE")+cCodPlan,"ZE_COBERT")
	cGrpDesc	:= Posicione("SZD",1,xFilial("SZD")+cGrpPlan,"ZD_DESCR")
	
	cQuery		:= "SELECT SZK.ZK_CODIGO, SZK.ZK_DESCR, SZK.ZK_LIMIDAD FROM " + RetSqlName("SZK") + " SZK "
	cQuery		+= "WHERE SZK.ZK_CODGRP = '" + cGrpPlan + "' "
	cQuery		+= "AND SZK.D_E_L_E_T_ = ' ' "
	cQuery		+= "ORDER BY SZK.ZK_LIMIDAD"
	
	If Select("SZK") <> 0
		SZK->(dbCloseArea())
	EndIf
	
	DbUseArea(.T., 'TOPCONN', TCGenQry(,,cQuery), 'SZK', .F.,.T.)
	
	DbSelectArea("SZK")
	SZK->(DbGoTop())
	
	While SZK->(!EOF())
		cCodDFx		:= SZK->ZK_CODIGO
		cDescrFx	:= SZK->ZK_DESCR
		nLimIdade	:= SZK->ZK_LIMIDAD
		
		If nLimIdade >= nIdade
			Exit
		EndIf
		
		SZK->(DbSkip())
	End
	
	SZK->(dbCloseArea())
	
	cQuery		:= "SELECT SZF.ZF_CODIGO, SZF.ZF_VALOR, SZF.ZF_VLSOS, SZF.ZF_VLAERO, SZF.ZF_FUNDO, SZF.ZF_TXADM, SZF.ZF_PERINSS, SZF.ZF_PERFUNC "
	cQuery		+= " FROM " + RetSqlName("SZF") + " SZF "
	cQuery		+= "WHERE SZF.ZF_CODGRP = '" + cGrpPlan + "' "
	cQuery		+= "AND SZF.ZF_CODPLAN = '" + cCodPlan + "' "
	cQuery		+= "AND SZF.ZF_CODDESC = '" + cCodDFx + "' "
	cQuery		+= "AND SZF.D_E_L_E_T_ = ' '"

	If Select("SZF") <> 0
		SZF->(dbCloseArea())
	EndIf
	
	DbUseArea(.T., 'TOPCONN', TCGenQry(,,cQuery), 'SZF', .F.,.T.)
	
	DbSelectArea("SZF")
	SZF->(DbGoTop())
	
	While SZF->(!EOF())
		cCodFx		:= SZF->ZF_CODIGO
		nValor		:= SZF->ZF_VALOR
		nVlSOS		:= SZF->ZF_VLSOS
		nVlAero		:= SZF->ZF_VLAERO
		nFundo		:= SZF->ZF_FUNDO
		nTxAdm		:= SZF->ZF_TXADM
		nPerInss	:= SZF->ZF_PERINSS
		nPerFunc	:= SZF->ZF_PERFUNC
		
		Exit
	End

	SZF->(dbCloseArea())
	
	aAdd(aPlano,cCodFx)		// [1] Código da Faixa
	aAdd(aPlano,cDescrFx)	// [2] Descricao da Faixa
	aAdd(aPlano,cGrpPlan)	// [3] Código do Grupo
	aAdd(aPlano,cGrpDesc)	// [4] Descricao do Grupo
	aAdd(aPlano,cCodPlan)	// [5] Código do Plano
	aAdd(aPlano,cDescPlan)	// [6] Descricao do Plano
	aAdd(aPlano,nValor)		// [7] Valor
	aAdd(aPlano,nVlSOS)		// [8] Valor SOS
	aAdd(aPlano,nVlAero)	// [9] Valor Aero
	aAdd(aPlano,nFundo)		// [10] Fundo
	aAdd(aPlano,nTxAdm)		// [11] Taxa Administrativa
	aAdd(aPlano,nPerInss)	// [12] Percentual INSS
	aAdd(aPlano,nPerFunc)	// [13] Percentual Funcionario
	aAdd(aPlano,nLimIdade)	// [14] Limite de Idade
	aAdd(aPlano,nCobPlan)	// [15] Cobertura do Plano
	
	RestArea(aArea)

Return(aPlano)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! CalcCoPar	! Autor !Gilson Lima 		 ! Data ! 17/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao cálculo da participação da família e Competência		 !
!           ! informada. 													 !
+----------------------------------------------------------------------------*/
User Function CalcCoPar(cFamilia,cCompet)

	Local aArea		:= GetArea()
	Local cChave 	:= xFilial("SZ1")+cFamilia
	Local cCompFat	:= Substr(cCompet,4,4) + Substr(cCompet,1,2)
	Local aCoPart	:= {}
	
	dbSelectArea("SZ1")
	SZ1->(dbSetOrder(1))
	SZ1->(dbGoTop())
	
	SZ1->(dbSeek(cChave))
	
	While SZ1->(!EOF()) .And. SZ1->(Z1_FILIAL+Z1_FAMILIA) == cChave
		If (SZ1->Z1_COMPFAT <= cCompFat) .And. (SZ1->Z1_PROCESS == 'N')
			aAdd(aCoPart,{;
				SZ1->(Recno()),;	// [1] - Nr Registro BD
				SZ1->Z1_FAMILIA,;	// [2] - Família
				SZ1->Z1_CDBENEF,;	// [3] - Código do Beneficiário
				SZ1->Z1_NMBENEF,;	// [4] - Nome do Beneficiário
				SZ1->Z1_DTATEND,;	// [5] - Data de Atendimento
				SZ1->Z1_DESGRAU,;	// [6] - Tipo de Atendimento
				SZ1->Z1_DESPROC,;	// [7] - Descrição do Atendimento
				SZ1->Z1_NMPREST,;	// [8] - Nome do Prestador
				SZ1->Z1_VLSERV,;	// [9] - Valor do Serviço
				SZ1->Z1_DEVCOB;		// [10]- Devolução ou Cobrança
				})
		EndIf
	
		SZ1->(dbSkip())
	End
	
	SZ1->(dbCloseArea())
	
	RestArea(aArea)
Return(aCoPart)
