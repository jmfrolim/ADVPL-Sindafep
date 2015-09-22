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
!Nome 			   ! CONTRFENAFISCO											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rel. de Contribuição Fenafisco - Mensalidades Fiscais	 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 25/02/2015												 !
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
! Programa 	! GRELFEN	! Autor !Gilson Lima 		 ! Data ! 10/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Tela da Rotina do Relatório de Contribuição Fenafisco			 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function GRelFen()

	Private cPerg		:= "GRELFEN"
	Private aPergs		:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Data Inicio:","","","mv_ch1","D",8,0,0,"G","NAOVAZIO()","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Data Fim:","","","mv_ch2","D",8,0,0,"G","NAOVAZIO()","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})

		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	If (!Pergunte(cPerg,.T.))
		Return
	EndIf

	//Chama função geração do relatório
	If MsgYesNo("Confirma a Geração do Relatório?","Atenção")
		Processa({||U_GrRelFen()}) 
	Endif 
	
Return()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GRRELFEN	! Autor !Gilson Lima 		 ! Data ! 25/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para geração do Relatório de Contribuição das		 	 !
! 			! Fenafisco das mensalidades dos fiscais						 !
+----------------------------------------------------------------------------*/
User Function GrRelFen()

	Local	lGerRel		:= .F.
	
	Private nDebQtd		:= 0 	
	Private nDebVal		:= 0 	
	Private nFolQtd		:= 0 	
	Private nFolVal		:= 0 	
	Private nOutrosQtd	:= 0 	
	Private nOutrosVal	:= 0 	
	
	cQuery := "SELECT SE1.E1_TIPO, SE1.E1_VALOR"
	cQuery += " FROM " + RetSqlName("SE1") + " SE1"
	cQuery += " WHERE SE1.D_E_L_E_T_ <> '*'"
	cQuery += " AND SE1.E1_PREFIXO IN ('MSL', 'MS1', 'MS2', 'MS3')"
	cQuery += " AND SE1.E1_VENCTO >= '" + DtoS(MV_PAR01) + "'"
	cQuery += " AND SE1.E1_VENCTO <= '" + DtoS(MV_PAR02) + "'"
	
	If Select('TMP') <> 0
		dbSelectArea('TMP')
		TMP->(dbCloseArea())
	EndIf
	
	dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TMP',.F.,.F.)
	
	dbSelectArea('TMP')
	TMP->(dbGoTop())

	While TMP->(!EOF())
	
		lGerRel := .T.

		Do Case
			// Débito em Conta Corrente
			Case TMP->E1_TIPO == 'DEB'
				
				nDebQtd++
				nDebVal += TMP->E1_VALOR
			
			// Desconto em Folha de Pagamento
			Case TMP->E1_TIPO == '5MH'
			
				nFolQtd++
				nFolVal += TMP->E1_VALOR
			
			// Cobrança Bancária / Financeiro
			OtherWise
			
				nOutrosQtd++
				nOutrosVal += TMP->E1_VALOR
		EndCase
				
		TMP->(dbSkip())		
	End
	
	TMP->(dbCloseArea())
	
	If lGerRel
		GerRelat()
	Else
		Alert("Nenhuma informação encontrada com os parâmetro informados. Relatório não gerado.")
	EndIf	

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERRELAT	! Autor !Gilson Lima 		 ! Data ! 02/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para geração do Relatório de Repasse das mensalidades 	 !
! 			! dos fiscais													 !
+----------------------------------------------------------------------------*/
Static Function GerRelat()

	Local	nPercContr 	:= GetNewPar('MV_PFENAF',  6)
	
	Private oRel		:= Nil
	Private nLinha		:= 0	// Altura da Linha
	Private	nLFinal	    := 3170 // Altura do Relatório
	Private nPage		:= 0	// Número de Páginas

	Private oFont9		:= TFont():New("Arial",9,9,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont9b		:= TFont():New("Arial",9,9,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont9m		:= TFont():New("Courier New",9,9,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont9mb	:= TFont():New("Courier New",9,9,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont10		:= TFont():New("Arial",9,10,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont10b	:= TFont():New("Arial",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont10m	:= TFont():New("Courier New",9,10,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont10mb	:= TFont():New("Courier New",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont12b	:= TFont():New("Arial",9,12,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont14		:= TFont():New("Arial",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont18		:= TFont():New("Arial",9,18,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont24		:= TFont():New("Arial",9,24,.T.,.T.,5,.T.,5,.T.,.F.)

	oRel := TMSPrinter():New("Relatório de Contribuição Fenafisco")
	oRel:Setup()
	oRel:SetPortrait()
	
	// Define valores
	
	// Total de Ítens Encontrados
	nTGQtd := nDebQtd + nFolQtd + nOutrosQtd
	cTGQtd := Transform(nTGQtd,"@E 99,999")
	
	// Soma valores
	nTGVal := nDebVal + nFolVal + nOutrosVal
	cTGVal := Transform(nTGVal,"@E 99,999,999.99")
	
	// Custo Operacional do Mês (5MH)
	nCOper5MH := 0
	cCOper5MH := Transform(nCOper5MH,"@E 99,999,999.99")	

	// 1/3 Reserva
	nTerco := nTGVal / 3
	cTerco := Transform(nTerco,"@E 99,999,999.99")
			
	// Valor Total (Soma - Custo Operacional)
	nTGValL := nTerco - nCOper5MH
	cTGValL := Transform(nTGValL,"@E 99,999,999.99")
	
	// Valor Contribuição
	nContrib := nTGValL * (nPercContr / 100)
	cContrib := Transform(nContrib,"@E 99,999,999.99")	
	
	// Dados para o cabeçalho
	cTitulo   := "Sindafep - Contribuição Fenafisco"	
	cPeriodo  := DtoC(MV_PAR01) + " à " + DtoC(MV_PAR02) 
	
	CabRelat(cTitulo,cPeriodo)
	
	If nDebQtd > 0
	
		cDebQtd		:= Transform(nDebQtd,"@E 99,999")
		cDebVal		:= Transform(nDebVal,"@E 99,999,999.99")
		
		oRel:Say(nLinha,0400,cDebQtd,oFont10mb)
		oRel:Say(nLinha,0600,'Previsão de Recebimento',oFont10)
		oRel:Say(nLinha,1200,'Débito em Conta Corrente',oFont10b)
		oRel:Say(nLinha,1900,cDebVal,oFont10mb)
		
		nLinha += 100
	
	EndIf
	
	If nFolQtd > 0

		cFolQtd		:= Transform(nFolQtd,"@E 99,999")
		cFolVal		:= Transform(nFolVal,"@E 99,999,999.99")

		oRel:Say(nLinha,0400,cFolQtd,oFont10mb)
		oRel:Say(nLinha,0600,'Previsão de Recebimento',oFont10)
		oRel:Say(nLinha,1200,'Desconto em Folha de Pagamento',oFont10b)
		oRel:Say(nLinha,1900,cFolVal,oFont10mb)
		
		nLinha += 100
			
	EndIf
	
	If nOutrosQtd > 0

		cOutrosQtd	:= Transform(nOutrosQtd,"@E 99,999")
		cOutrosVal	:= Transform(nOutrosVal,"@E 99,999,999.99")

		oRel:Say(nLinha,0400,cOutrosQtd,oFont10mb)
		oRel:Say(nLinha,0600,'Previsão de Recebimento',oFont10)
		oRel:Say(nLinha,1200,'Cobrança Bancária / Financeiro',oFont10b)
		oRel:Say(nLinha,1900,cOutrosVal,oFont10mb)
		
		nLinha += 100
			
	EndIf
	
	oRel:Line(nLinha,1900,nLinha,2230)
	
	nLinha += 50
	
	oRel:Say(nLinha,1200,'TOTAL GERAL: '+cTGQtd,oFont10b)
	oRel:Say(nLinha,1900,cTGVal,oFont10mb)
	
	nLinha += 200
	
	cDescrL1	:= 'Artigo 69 do estatuto prevê a reserva de 1/3 do valor'
	cDescrL2	:= 'das mensalidades recebidas, que serão destinadas para'
	cDescrL3	:= 'fins Sindicais.'
	
	oRel:Say(nLinha,0200,cDescrL1,oFont10)
	
	nLinha += 60
	
	oRel:Say(nLinha,0200,cDescrL2,oFont10)
	
	nLinha += 60
	
	oRel:Say(nLinha,0200,cDescrL3,oFont10)
	oRel:Say(nLinha,1900,cTerco,oFont10m)
	
	nLinha += 120
	
	oRel:Say(nLinha,0200,'Custo Operacional do Mês (5MH)' + Replicate('.',109) + ':',oFont10)
	oRel:Say(nLinha,1900,cCOper5MH,oFont10m)
	
	nLinha += 80

	oRel:Line(nLinha,1900,nLinha,2230)
	
	nLinha += 40	
	
	oRel:Say(nLinha,0200,'Valor Total' + Replicate('.',146) + ':',oFont10)
	oRel:Say(nLinha,1900,cTGValL,oFont10m)
	
	nLinha += 80
	
	oRel:Say(nLinha,0200,'Contribuição de ' + cValToChar(nPercContr) + '%' + Replicate('.',131) + ':',oFont10)
	oRel:Say(nLinha,1900,cContrib,oFont10m)
	
	// Rodapé
	RodRelat()
	
	// Finaliza Página
	oRel:EndPage()

	// Preview da Página	
	oRel:Preview()	

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! CABRELAT	! Autor !Gilson Lima 		 ! Data ! 24/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Imprime Cabeçalho do Relatório								 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function CabRelat(cTitulo,cPeriodo)


	Local cLogo :='\system\sindafep.bmp'
	
	nPage++

	oRel:StartPage()

	nLinha := 50

	If (File(cLogo))
		oRel:SayBitmap(nLinha+35,100,cLogo,310,200)
	EndIf
	
	nLinha += 100
	oRel:Say(nLinha,1000,cTitulo,oFont14)

	nLinha += 80
	oRel:Say(nLinha,1180,cPeriodo,oFont10b)
	
	
	nLinha += 150

	oRel:Line(nLinha,0100,nLinha,2300)

	nLinha += 250
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RODRELAT	! Autor !Gilson Lima 		 ! Data ! 24/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Imprime Rodapé do Relatório								 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function RodRelat()

	oRel:Line(nLFinal + 70,0100,nLFinal + 70,2300)
	oRel:Say(nLFinal + 90,0100,'SINDAFEP - Sindicato dos Auditores Fiscais da Receita do Estado do Paraná',oFont9)
	oRel:Say(nLFinal + 90,1800,DtoC(dDataBase) + '  ' + Time() + '  Página: ' + cValToChar(nPage),oFont9)

Return
