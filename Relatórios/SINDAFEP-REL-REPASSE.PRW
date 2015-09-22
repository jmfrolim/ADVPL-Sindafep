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
!Nome 			   ! REL-REPASSE											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Relatório de repasses - Mensalidades Fiscais			 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 29/12/2014												 !
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
! Programa 	! GRELREP	! Autor !Gilson Lima 		 ! Data ! 10/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Tela da Rotina do Relatóriode Repasses						 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function GRelRep()

	Private cPerg		:= "GRELREP"
	Private aPergs		:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Data Inicio:","","","mv_ch1","D",8,0,0,"G","NAOVAZIO()","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Data Fim:","","","mv_ch2","D",8,0,0,"G","NAOVAZIO()","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Todas as DRRs?","","","mv_ch3","C",1,0,0,"C","","MV_PAR03","Sim","","","","","Não","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Selecione DRR:","","","mv_ch4","C",6,0,0,"G","","MV_PAR04","","","","000001","","","","","","","","","","","","","","","","","","","","","SZ2","","","",""})
		AADD(aPergs,{"Tipo de Relatório:","","","mv_ch5","C",1,0,0,"C","","MV_PAR05","Analítico","","","","1","Sintético","","","","","","","","","","","","","","","","","","","","","","",""})

		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	If (!Pergunte(cPerg,.T.))
		Return
	EndIf

	//Chama função geração de títulos
	If MsgYesNo("Confirma a Geração do Relatório?","Atenção")
		Processa({||U_GrRelRep()}) 
	Endif 
	
Return()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GRRELREP	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para geração do Relatório de Repasse das mensalidades 	 !
! 			! dos fiscais													 !
+----------------------------------------------------------------------------*/
User Function GrRelRep()

	Local cDDR	 	:= ''
	Local cQuery 	:= ''
	Local aDados 	:= {}
	Local aDadosTit	:= {}
	Local nContDDR  := 0
	Local nContFis	:= 0
	Local nTotDDR   := 0
	Local nTotGer	:= 0
	Local nTotTit	:= 0
	Local aSitFisc  := StrToArray(GetNewPar('MV_SITFISC',  'FA,NA,FAP'),',')
	
	Private lAnalitico := IIF(MV_PAR05 == 1, .T., .F.)	// Define se é analítico ou sintético
	
	cQuery := "SELECT SE1.E1_NUM, SE1.E1_HIST, SE5.E5_MOTBX, SE5.E5_DTDISPO, SE5.E5_VALOR, SA1.A1_NOME," 
	cQuery += " SZ2.Z2_CODIGO, SZ2.Z2_DESCRIC, SZ2.Z2_PREPASS, SZ2.Z2_PSEAD, SZ2.Z2_CBANCO, SZ2.Z2_CBOLETO, SZ2.Z2_GREPASS"
	cQuery += " FROM " + RetSqlName('SE1') + " SE1, " + RetSqlName('SE5') + " SE5, " + RetSqlName('SA1') + " SA1, " + RetSqlName('SZ2') + " SZ2"
	cQuery += " WHERE (SE1.E1_PREFIXO = SE5.E5_PREFIXO) AND (SE1.E1_NUM = SE5.E5_NUMERO)"
	cQuery += " AND (SE1.E1_CLIENTE = SA1.A1_COD) AND (SE1.E1_LOJA = SA1.A1_LOJA)"

	// Busca apenas fiscais informados no parâmetro
	For nAB := 1 To Len(aSitFisc)
	
		If nAB == 1
			cQuery += " AND ("
		EndIf
		
		cQuery += "(SA1.A1_XSITDFI = '" + PadR(AllTrim(aSitFisc[nAB]),TamSX3("A1_XSITDFI")[01],' ') + "')"
		
		If nAb != Len(aSitFisc)
			cQuery += " OR "
		EndIF
		
		iF nAB == Len(aSitFisc)
			cQuery += ")"
		EndIf
	
	Next nAB
	
	cQuery += " AND (SA1.A1_XDEPART = SZ2.Z2_CODIGO)"
	cQuery += " AND (SE5.E5_PREFIXO IN ('MSL', 'MS1', 'MS2', 'MS3'))"
	cQuery += " AND (SE5.E5_TIPODOC = 'VL')"
	cQuery += " AND (SE5.E5_DTDISPO >= '" + DtoS(MV_PAR01) + "') AND (SE5.E5_DTDISPO <= '" + DtoS(MV_PAR02) + "')"
	cQuery += " AND (SZ2.Z2_GREPASS = 'S')"
	
	If MV_PAR03 != 1
		cQuery += " AND (SZ2.Z2_CODIGO = '" + MV_PAR04 + "')"
	EndIf
	
	cQuery += " ORDER BY SZ2.Z2_CODIGO, SA1.A1_NOME, SE1.E1_NUM"

	If Select('TMP') <> 0
		dbSelectArea('TMP')
		TMP->(dbCloseArea())
	EndIf
	
	dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TMP',.F.,.F.)
	
	dbSelectArea('TMP')
	TMP->(dbGoTop())

	While TMP->(!EOF())

		If (AllTrim(cDDR) == '')
			cDDR 	:= TMP->Z2_CODIGO
			nContDDR++
			aAdd(aDados,Array(9))
			aDadosTit := {}
		ElseIf (cDDR != TMP->Z2_CODIGO)
			cDDR := TMP->Z2_CODIGO
			nContFis := 0
			nTotDDR  := 0
			nContDDR++
			aAdd(aDados,Array(9))
			aDadosTit := {}
		EndIf
		nContFis ++
		nTotDDR += TMP->E5_VALOR
		nTotGer += TMP->E5_VALOR
		nTotTit ++
		
		aAdd(aDadosTit,{TMP->A1_NOME,TMP->E1_HIST,TMP->E5_VALOR,TMP->E5_MOTBX})
		
		aDados[nContDDR][1] := TMP->Z2_CODIGO
		aDados[nContDDR][2] := TMP->Z2_DESCRIC
		aDados[nContDDR][3] := TMP->Z2_PREPASS
		aDados[nContDDR][4] := TMP->Z2_PSEAD
		aDados[nContDDR][5] := TMP->Z2_CBANCO
		aDados[nContDDR][6] := TMP->Z2_CBOLETO
		aDados[nContDDR][7] := nContFis
		aDados[nContDDR][8] := nTotDDR
		aDados[nContDDR][9]	:= aDadosTit
		
		TMP->(dbSkip())		
	End
	
	TMP->(dbCloseArea())
	
	If Len(aDados) > 0
		GerRelat(aDados)
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
Static Function GerRelat(aDados)

	Local	aGeral		:= {}
	
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

	oRel := TMSPrinter():New("Relatório de Repasse")
	oRel:Setup()
	oRel:SetPortrait()

	// Relatório Analítico
	If lAnalitico
	
		For Nx := 1 To Len(aDados)
	
			cTitulo   := "Sindafep - Relatório de Repasse Analítico"	
			cDDR	  := aDados[nX][2]
			cPeriodo  := DtoC(MV_PAR01) + " à " + DtoC(MV_PAR02) 
			nTReceb	  := 0
			nTRepasse := 0
			nTTxBanc  := 0
			nTLiquido := 0
			nTItens	  := 0
	
			CabRelat(cTitulo,cDDR,cPeriodo)
			CabecLin()
	
			For nY := 1 To Len (aDados[nX][9])
				
				cNome		:= Substr(aDados[nX][9][nY][1],1,30)
				cHist		:= aDados[nX][9][nY][2]
	
				If aDados[nX][9][nY][4] $ "DEB"
					nVlBco	:= aDados[nX][5]
				ElseIf aDados[nX][9][nY][4] $ "BOL"
					nVlBco	:= aDados[nX][6]
				Else
					nVlBco	:= 0
				EndIf
							
				nVlRec		:= aDados[nX][9][nY][3]
				nVlRep		:= aDados[nX][9][nY][3]*(aDados[nX][3]/100)
				nVlLiq		:= nVlRep - nVlBco
				
				nTReceb 	+= nVlRec
				nTRepasse 	+= nVlRep
				nTTxBanc  	+= nVlBco
				nTLiquido  	+= nVlLiq
				
				cVlRec		:= Transform(nVlRec,"@E 999,999.99")
				cVlRep		:= Transform(nVlRep,"@E 999,999.99")
				cVlBco		:= Transform(nVlBco,"@E 999,999.99")
				cVlLiq		:= Transform(nVlLiq,"@E 999,999.99")
				
				If nLinha > nLFinal
				
					// Imprime Rodapé
					RodRelat()
					oRel:EndPage()
					
					// Inicia nova página
					CabRelat(cTitulo,cDDR,cPeriodo)
					CabecLin()
				EndIf
		
				oRel:Say(nLinha,0100,StrZero(nY,4),oFont9)
				oRel:Say(nLinha,0200,cNome,oFont9)
				oRel:Say(nLinha,0950,cHist,oFont9)
				oRel:Say(nLinha,1350,cVlRec,oFont9m)
				oRel:Say(nLinha,1600,cVlRep,oFont9m)
				oRel:Say(nLinha,1850,cVlBco,oFont9m)
				oRel:Say(nLinha,2100,cVlLiq,oFont9m)
				
				nLinha += 60
				
				If (nY == Len(aDados[nX][9]))
					
					nLinha += 20
					oRel:Line(nLinha,1350,nLinha,2300)
	
					nLinha += 20
					oRel:Say(nLinha,0200,"Total " + cDDR,oFont9b)
					oRel:Say(nLinha,1350,Transform(nTReceb,"@E 999,999.99"),oFont9mb)
					oRel:Say(nLinha,1600,Transform(nTRepasse,"@E 999,999.99"),oFont9mb)
					oRel:Say(nLinha,1850,Transform(nTTxBanc,"@E 999,999.99"),oFont9mb)
					oRel:Say(nLinha,2100,Transform(nTLiquido,"@E 999,999.99"),oFont9mb)
					
					// Imprime Rodapé
					RodRelat()
						
				EndIf
			
			Next nY
	
			oRel:EndPage()		
			
		Next Nx	
		
		// Define Analítico como .F. para impressão do Sintético
		lAnalitico := .F.
	
	EndIf

	// Relatório Sintético
	If !lAnalitico
		
		For Nx := 1 To Len(aDados)
		
			nTReceb	  := 0
			nTRepasse := 0
			nTTxBanc  := 0
			nTLiquido := 0
			nTItens	  := 0

			cDDR	  := aDados[nX][2]
			nPercRep  := aDados[nX][3]
	
			For nY := 1 To Len (aDados[nX][9])
				
	
				If aDados[nX][9][nY][4] $ "DEB"
					nVlBco	:= aDados[nX][5]
				ElseIf aDados[nX][9][nY][4] $ "BOL"
					nVlBco	:= aDados[nX][6]
				Else
					nVlBco	:= 0
				EndIf
							
				nVlRec		:= aDados[nX][9][nY][3]
				nVlRep		:= aDados[nX][9][nY][3]*(aDados[nX][3]/100)
				nVlLiq		:= nVlRep - nVlBco
				
				nTReceb 	+= nVlRec
				nTRepasse 	+= nVlRep
				nTTxBanc  	+= nVlBco
				nTLiquido  	+= nVlLiq
				nTItens++
			Next nY
			
			// Adiciona valores ao Array de dados Sintéticos
			aAdd(aGeral,{cDDR,nTReceb,nTRepasse,nTTxBanc,nTLiquido,nTItens,nPercRep})
		Next nX
		
		// Zera valores Totais Gerais
		nTGReceb 	:= 0
		nTGRepasse 	:= 0
		nTGTxBanc  	:= 0
		nTGLiquido  := 0
		nTGItens	:= 0
				
		cTitulo   := "Sindafep - Relatório de Repasse Sintético"	
		cPeriodo  := DtoC(MV_PAR01) + " à " + DtoC(MV_PAR02) 

		CabRelat(cTitulo,'',cPeriodo)
		CabLinS()
							
		For nZ := 1 To Len(aGeral)
			
			// Adiciona valores aos totais gerais
			nTGReceb 	+= aGeral[nZ][2]
			nTGRepasse 	+= aGeral[nZ][3]
			nTGTxBanc  	+= aGeral[nZ][4]
			nTGLiquido  += aGeral[nZ][5]
			nTGItens	+= aGeral[nZ][6]
			
			// Transforma e String com máscara	
			cVlRec		:= Transform(aGeral[nZ][2],"@E 99,999,999.99")
			cVlRep		:= Transform(aGeral[nZ][3],"@E 99,999,999.99")
			cVlBco		:= Transform(aGeral[nZ][4],"@E 99,999,999.99")
			cVlLiq		:= Transform(aGeral[nZ][5],"@E 99,999,999.99")
			cTItens		:= Transform(aGeral[nZ][6],"@E 99,999")
			cPRep		:= Transform(aGeral[nZ][7],"@E 999.99")+'%'
			

			If nLinha > nLFinal
			
				// Imprime Rodapé
				RodRelat()
				oRel:EndPage()
				
				// Inicia nova página
				CabRelat(cTitulo,'',cPeriodo)
				CabLinS()
			EndIf
	
			oRel:Say(nLinha,0100,cTItens,oFont9m)
			oRel:Say(nLinha,0200,aGeral[nZ][1],oFont9)
			oRel:Say(nLinha,0850,cVlRec,oFont9m)
			oRel:Say(nLinha,1100,cPRep,oFont9m)
			oRel:Say(nLinha,1300,cVlRep,oFont9m)
			oRel:Say(nLinha,1650,cVlBco,oFont9m)
			oRel:Say(nLinha,2000,cVlLiq,oFont9m)
			
			nLinha += 60
			
			If (nZ == Len(aGeral))
				
				nLinha += 20
				oRel:Line(nLinha,0850,nLinha,2300)

				nLinha += 20
				oRel:Say(nLinha,0100,Transform(nTGItens,"@E 99,999"),oFont9mb)
				oRel:Say(nLinha,0200,"Total :",oFont9b)
				oRel:Say(nLinha,0850,Transform(nTGReceb,"@E 99,999,999.99"),oFont9mb)
				oRel:Say(nLinha,1300,Transform(nTGRepasse,"@E 99,999,999.99"),oFont9mb)
				oRel:Say(nLinha,1650,Transform(nTGTxBanc,"@E 99,999,999.99"),oFont9mb)
				oRel:Say(nLinha,2000,Transform(nTGLiquido,"@E 99,999,999.99"),oFont9mb)
				
				// Imprime Rodapé
				RodRelat()
					
			EndIf
		Next Nx	

		oRel:EndPage()		
	
	EndIf
	
	oRel:Preview()	

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! CABRELAT	! Autor !Gilson Lima 		 ! Data ! 24/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Imprime Cabeçalho do Relatório								 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function CabRelat(cTitulo,cDDR,cPeriodo)


	Local cLogo :='\system\sindafep.bmp'
	
	nPage++

	oRel:StartPage()

	nLinha := 0

	If (File(cLogo))
		oRel:SayBitmap(nLinha+35,100,cLogo,310,200)
	EndIf
	
	nLinha += 50
	oRel:Say(nLinha,1000,cTitulo,oFont14)

	nLinha += 80
	oRel:Say(nLinha,1180,cPeriodo,oFont10b)
	
	nLinha += 150
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! CABCLIN	! Autor !Gilson Lima 		 ! Data ! 24/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Imprime Cabeçalho de Ítens									 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function CabecLin()
	

	oRel:Say(nLinha,0100,cDDR,oFont12b)
	
	nLinha += 100
	oRel:Say(nLinha,0200,"Nome",oFont9b)
	oRel:Say(nLinha,0950,"Descrição",oFont9b)
	oRel:Say(nLinha,1350,"Vl. Recebido",oFont9b)
	oRel:Say(nLinha,1600,"Vl. Repasse",oFont9b)
	oRel:Say(nLinha,1850,"C. Operacional",oFont9b)
	oRel:Say(nLinha,2100,"Vl. Líquido",oFont9b)
	
	nLinha += 50
	oRel:Line(nLinha,0100,nLinha,2300)
	
	nLinha += 30
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! CABLINS	! Autor !Gilson Lima 		 ! Data ! 24/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Imprime Cabeçalho de Ítens									 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function CabLinS()
	
	nLinha += 100
	oRel:Say(nLinha,0100,"Qtd.",oFont9b)
	oRel:Say(nLinha,0200,"DDR",oFont9b)
	oRel:Say(nLinha,0850,"Vl. Recebido",oFont9b)
	oRel:Say(nLinha,1100,"Perc.",oFont9b)
	oRel:Say(nLinha,1300,"Vl. Repasse",oFont9b)
	oRel:Say(nLinha,1650,"C. Operacional",oFont9b)
	oRel:Say(nLinha,2000,"Vl. Líquido",oFont9b)
	
	nLinha += 50
	oRel:Line(nLinha,0100,nLinha,2300)
	
	nLinha += 30
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
