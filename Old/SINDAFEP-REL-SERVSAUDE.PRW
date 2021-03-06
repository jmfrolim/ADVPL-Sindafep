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
!Nome 			   ! REL-SERVSAUDE											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Relat�rio de Servi�os de Sa�de							 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 19/02/2015												 !
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
! Programa 	! GRELPLN	! Autor !Gilson Lima 		 ! Data ! 19/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Tela da Rotina do Relat�rio de Servi�os de Sa�de				 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function GRelPln()

	Private cPerg		:= "GRELPLN"
	Private aPergs		:= {}
	
	// Cria pergunta caso n�o exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		// Compet�ncia
		AADD(aPergs,{"Compet�ncia:","","","mv_ch1","C",7,0,0,"G","NAOVAZIO()","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 99/9999"})
		// Titulares
		AADD(aPergs,{"Todos os Titulares?","","","mv_ch2","C",1,0,0,"C","","MV_PAR02","Sim","","","","","N�o","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Selecione o Titular:","","","mv_ch3","C",99,0,0,"G","","MV_PAR03","","","","000001","","","","","","","","","","","","","","","","","","","","","SA1PLN","","","",""})
		// Planos
		AADD(aPergs,{"Todos os Planos?","","","mv_ch4","C",1,0,0,"C","","MV_PAR04","Sim","","","","","N�o","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Selecione o Plano:","","","mv_ch5","C",99,0,0,"G","","MV_PAR05","","","","000001","","","","","","","","","","","","","","","","","","","","","SZDGRP","","","",""})
		// Fam�lias
		AADD(aPergs,{"Todos as Fam�lias?","","","mv_ch6","C",1,0,0,"C","","MV_PAR06","Sim","","","","","N�o","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Selecione a Fam�lia:","","","mv_ch7","C",99,0,0,"G","","MV_PAR07","","","","000001","","","","","","","","","","","","","","","","","","","","","SZHFAM","","","",""})
		// Forma de Cobran�a
		AADD(aPergs,{"Considerar Forma Cobr?","","","mv_ch8","C",1,0,0,"C","","MV_PAR08","Sim","","","1","","Nao","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Forma de Cobran�a","","","mv_ch9","C",1,0,0,"C","","MV_PAR09","Financeiro","","","1","","Folha","","","","","D�bito Autom�tico","","","","","Boleto","","","","","Fatura","","","","","","","",""})
		// Co-Participa��o
		AADD(aPergs,{"Detalha Co-Participa��o?","","","mv_ch10","C",1,0,0,"C","","MV_PAR10","Sim","","","1","","Nao","","","","","","","","","","","","","","","","","","","","","","",""})

		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)

	//Chama fun��o gera��o de t�tulos
	If MsgYesNo("Confirma a Gera��o do Relat�rio?","Aten��o")
		Processa({||U_GrRelPln()}) 
	Endif 
	
Return()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GrRelPln	! Autor !Gilson Lima 		 ! Data ! 19/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para gera��o do Relat�rio de Servi�os de Sa�de		 	 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function GrRelPln()

	Local	cQuery		:= ''
	Local	aDadosTit	:= {}
	Local	aDadosPl	:= {}
	Local	aDados		:= {}
	Local	cFormCobr	:= ''
	Local	cTitPref	:= ''
		
	Private	cCompet		:= Substr(MV_PAR01,4,4) + Substr(MV_PAR01,1,2)	// Compet�ncia
	Private cCompRelat	:= MV_PAR01	// Compet�ncia Relat�rio
	Private	lTTitular	:= IIF(MV_PAR02 == 1, .T., .F.)
	Private aTitular	:= StrToKarr(AllTrim(MV_PAR03),'|')
	Private lTPlano		:= IIF(MV_PAR04 == 1, .T., .F.)
	Private cPlano		:= AllTrim(MV_PAR05)
	Private lTFamilia	:= IIF(MV_PAR06 == 1, .T., .F.)
	Private cFamilia	:= PadR(AllTrim(MV_PAR07),TamSX3('ZL_FAMILIA')[1],' ')
	Private lFormCobr	:= IIF(MV_PAR08 == 1, .T., .F.)
	Private nFormCobr	:= MV_PAR09 	// 1=Financeiro, 2=Folha, 3=D�bito Autom�tico, 4=Boleto, 5=Fatura
	Private lDetCoPart  := IIF(MV_PAR10 == 1, .T., .F.)
	
	cQuery	:= "SELECT SZL.ZL_FAMILIA, SZL.ZL_TITPREF, SZL.ZL_TITNUM, SZL.ZL_TITLOJA, SZL.ZL_TITTIPO,"
	cQuery	+= " SZL.ZL_TITEMIS, SZL.ZL_TITVNC, SZL.ZL_TITVNCR, SZL.ZL_TITVAL, SZL.ZL_TITCLI, SZL.ZL_TITHIST"
	cQuery	+= " FROM " + RetSqlName("SZL") + " SZL"
	cQuery	+= " WHERE SZL.D_E_L_E_T_ <> '*'"
	cQuery	+= " AND SZL.ZL_COMPET = '" + cCompet + "'"
	cQuery	+= " AND SZL.ZL_TPDADO = 'F'"	// Tipo Fatura
	
	// Titular
	If !lTTitular
		cQuery += " AND (SZL.ZL_TITCLI = '" + aTitular[1] + "'"		// C�digo
		cQuery += " AND SZL.ZL_TITLOJA = '" + aTitular[2] + "')"	// Loja
	EndIf
	
	// Grupo de Planos
	If !lTPlano
		Do Case
			Case cPlano == '000001'
				cTitPref := 'PLS'
			Case cPlano	== '000002'
				cTitPref := 'PLO'
		EndCase
		
		cQuery += " AND SZL.ZL_TITPREF = '" + cTitPref + "'"
	EndIf
	
	// Fam�lia
	If !lTFamilia
		cQuery += " AND SZL.ZL_FAMILIA = '" + cFamilia + "'"
	EndIf
	
	// Forma de Cobran�a
	If lFormCobr
		Do Case
			Case nFormCobr == 1
				cFormCobr  := PadR('RC' ,TamSX3("ZL_TITTIPO")[1],' ')
			Case nFormCobr == 2
				cFormCobr  := PadR('FOL',TamSX3("ZL_TITTIPO")[1],' ')
			Case nFormCobr == 3
				cFormCobr  := PadR('DEB',TamSX3("ZL_TITTIPO")[1],' ')
			Case nFormCobr == 4
				cFormCobr  := PadR('BOL',TamSX3("ZL_TITTIPO")[1],' ')
			Case nFormCobr == 5
				cFormCobr  := PadR('FT' ,TamSX3("ZL_TITTIPO")[1],' ')
		EndCase
		
		cQuery += " AND SZL.ZL_TITTIPO = '" + cFormCobr + "'"
	EndIf

	If Select('TMP') <> 0
		dbSelectArea('TMP')
		TMP->(dbCloseArea())
	EndIf
	
	dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TMP',.F.,.F.)
	
	dbSelectArea('TMP')
	TMP->(dbGoTop())

	While TMP->(!EOF())
		
		// Adiciona ao Array de Dados do T�tulo
		aAdd(aDadosTit,{;
			TMP->ZL_TITPREF,;
			TMP->ZL_TITNUM,;
			TMP->ZL_TITLOJA,;
			TMP->ZL_TITTIPO,;
			TMP->ZL_TITEMIS,;
			TMP->ZL_TITVNC,;
			TMP->ZL_TITVNCR,;
			TMP->ZL_TITVAL,;
			TMP->ZL_TITCLI,;
			TMP->ZL_TITHIST,;
			TMP->ZL_FAMILIA;
		})
		
		TMP->(dbSkip())		
	End
	
	TMP->(dbCloseArea())
	
	// Se n�o houver dados para gerar o relat�rio, informa e retorna
	If Len(aDadosTit) == 0	
		Alert("Nenhuma informa��o encontrada com os par�metro informados. Relat�rio n�o gerado.")
		Return
	EndIf
	
	// Grava Array com informa��es dos T�tulos
	For nA := 1 To Len(aDadosTit)
		// Busca dados dos planos e grava em array
		
		cQuery := "SELECT SZL.ZL_PLINTTP, SZL.ZL_PLINTOR, SZL.ZL_PLINTCD, SZL.ZL_PLINTLJ, SZL.ZL_PLINTNM, SZL.ZL_PLGRPCD, SZL.ZL_PLCOD, SZL.ZL_PLDESCR,"
		cQuery += " SZL.ZL_PLFXDSC, SZL.ZL_PLVL, SZL.ZL_PLVLCOB, SZL.ZL_PLVLFUN, SZL.ZL_PLVLTXA, SZL.ZL_PLPINSS, SZL.ZL_PLPFUNC,"
		cQuery += " SZL.ZL_PLINC24, SZL.ZL_PLCOB24, SZL.ZL_PLDIA24, SZL.ZL_PLVL24, SZL.ZL_ADCOD, SZL.ZL_ADDESCR, SZL.ZL_CPRECNO, SZL.ZL_CPBENCD, SZL.ZL_CPBENNM,"
		cQuery += " SZL.ZL_CPATDDT, SZL.ZL_CPATDTP, SZL.ZL_CPATDDS, SZL.ZL_CPPRNOM, SZL.ZL_CPVLSER, SZL.ZL_CPCOBDV, SZL.ZL_CPPINSS"
		cQuery += " FROM " + RetSqlName("SZL") + " SZL"
		cQuery += " WHERE SZL.D_E_L_E_T_ <> '*'"
		cQuery += " AND SZL.ZL_TPDADO <> 'F'"
		cQuery += " AND SZL.ZL_TITPREF = '" + aDadosTit[nA][1] + "'"
		cQuery += " AND SZL.ZL_TITNUM = '" + aDadosTit[nA][2] + "'"
		cQuery += " AND SZL.ZL_TITLOJA = '" + aDadosTit[nA][3] + "'"
		
		dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TMP',.F.,.F.)
		
		dbSelectArea('TMP')
		TMP->(dbGoTop())
		
		aDadosPl := {}
		aDadosCp := {}
		aIntegr	 := {}
	
		While TMP->(!EOF())
		
			// Se for plano de Sa�de
			If AllTrim(TMP->ZL_PLCOD) != ''
			
				cCod	:= TMP->ZL_PLCOD
				
				// Descri��o conforme o tipo de plano
				Do Case
					Case TMP->ZL_PLGRPCD == '000001'	// Unimed
						cDescr	:= 'PLANO SA�DE: '+ AllTrim(TMP->ZL_PLDESCR) + ' - ' + AllTrim(TMP->ZL_PLFXDSC)
					Case TMP->ZL_PLGRPCD == '000002'	// OdontoUni
						cDescr	:= 'PLANO ODONTOL�GICO: '+ AllTrim(TMP->ZL_PLDESCR)
				EndCase
				
				// Calcula valores
				nValor	:= TMP->ZL_PLVL
				
				// Se for titular e funcion�rio
				If TMP->ZL_PLINTTP == 'T' .And. TMP->ZL_PLINTOR == '000003'	
					nValor -= nValor * (TMP->ZL_PLPFUNC / 100)
				EndIf
				
				nTxAdm	:= TMP->ZL_PLVLTXA
				nFundo	:= TMP->ZL_PLVLFUN
				nINSS	:= nValor * (TMP->ZL_PLPINSS / 100)
				nTotal	:= nValor + nINSS + nTxAdm + nFundo
				
				// Verifica se j� existe este integrante no Array. Se sim, adiciona o plano, se n�o, adiciona o integrante e o Plano
				laIntegr := .F.
				If Len(aIntegr) > 0
					For nD := 1 To Len(aIntegr)
						If aIntegr[nD][1] == TMP->ZL_PLINTTP .And. aIntegr[nD][2] == TMP->ZL_PLINTOR .And. aIntegr[nD][3] == TMP->ZL_PLINTCD .And. aIntegr[nD][4] == TMP->ZL_PLINTLJ 
						
							aAdd(aIntegr[nD][6],{;
								cCod,;
								cDescr,;
								nValor,;
								nTxAdm,;
								nFundo,;
								nINSS,;
								nTotal;
							})
						
							laIntegr:= .T.
						EndIf
					Next nD
				EndIf
				
				// Se n�o existir o integrante
				If !laIntegr
					// Adiciona integrante
					aAdd(aIntegr,{;
						TMP->ZL_PLINTTP,;
						TMP->ZL_PLINTOR,;
						TMP->ZL_PLINTCD,;
						TMP->ZL_PLINTLJ,;
						AllTrim(TMP->ZL_PLINTNM),;
						{};
					})
					// Adiciona plano
					aAdd(aIntegr[Len(aIntegr)][6],{;
						cCod,;
						cDescr,;
						nValor,;
						nTxAdm,;
						nFundo,;
						nINSS,;
						nTotal;
					})
				EndIf
				
				// Se for cobran�a 24H
				If TMP->ZL_PLCOB24 == 'S'
					
					// Descri��o conforme plano
					Do Case
						Case TMP->ZL_PLGRPCD == '000001'	// Unimed
							cDescr	:= 'PLANO SA�DE: Inclus�o 24H - ' + cValToChar(TMP->ZL_PLDIA24) + ' dias.'
						Case TMP->ZL_PLGRPCD == '000002'	// OdontoUni
							cDescr	:= 'PLANO ODONTOL�GICO: Inclus�o 24H - ' + cValToChar(TMP->ZL_PLDIA24) + ' dias.'
					EndCase
					
					// Calcula valores
					nValor	:= TMP->ZL_PLVL
					
					// Se for titular e funcion�rio executa o referido desconto
					If TMP->ZL_PLINTTP == 'T' .And. TMP->ZL_PLINTOR == '000003'	// Se for titular e funcion�rio
						nValor -= nValor * (TMP->ZL_PLPFUNC / 100)
					EndIf
					
					nValor := (nValor / 30) * TMP->ZL_PLDIA24
					nFundo := TMP->ZL_PLVLFUN 
					nTxAdm	:= TMP->ZL_PLVLTXA
					nINSS	:= nValor * (TMP->ZL_PLPINSS / 100)
					nTotal	:= nValor + nINSS + nTxAdm + nFundo
	
					// Verifica se j� existe este integrante no Array. Se sim, adiciona o plano, se n�o, adiciona o integrante e o Plano			
					laIntegr := .F.
					If Len(aIntegr) > 0
						For nD := 1 To Len(aIntegr)
							If aIntegr[nD][1] == TMP->ZL_PLINTTP .And. aIntegr[nD][2] == TMP->ZL_PLINTOR .And. aIntegr[nD][3] == TMP->ZL_PLINTCD .And. aIntegr[nD][4] == TMP->ZL_PLINTLJ 
							
								aAdd(aIntegr[nD][6],{;
									cCod,;
									cDescr,;
									nValor,;
									nTxAdm,;
									nFundo,;
									nINSS,;
									nTotal;
								})
							
								laIntegr:= .T.
							EndIf
						Next nD
					EndIf
					
					// Se n�o existir integrante
					If !laIntegr
						// Adiciona o integrante
						aAdd(aIntegr,{;
							TMP->ZL_PLINTTP,;
							TMP->ZL_PLINTOR,;
							TMP->ZL_PLINTCD,;
							TMP->ZL_PLINTLJ,;
							AllTrim(TMP->ZL_PLINTNM),;
							{};
						})
						// Adiciona o plano
						aAdd(aIntegr[Len(aIntegr)][6],{;
							cCod,;
							cDescr,;
							nValor,;
							nTxAdm,;
							nFundo,;
							nINSS,;
							nTotal;
						})
					EndIf
				
				EndIf
			
			// Se for Adicional
			ElseIf AllTrim(TMP->ZL_ADCOD) != ''
				
				// Define Descri��o e valores
				cCod	:= TMP->ZL_ADCOD
				cDescr	:= AllTrim(TMP->ZL_ADDESCR)
				nValor	:= TMP->ZL_PLVL
				nTxAdm	:= TMP->ZL_PLVLTXA
				nFundo	:= TMP->ZL_PLVLFUN
				nINSS	:= nValor * (TMP->ZL_PLPINSS / 100)
				nTotal	:= nValor + nINSS + nTxAdm + nFundo
				
				// Verifica se j� existe este integrante no Array. Se sim, adiciona o plano, se n�o, adiciona o integrante e o Adicional
				laIntegr := .F.
				If Len(aIntegr) > 0
					For nD := 1 To Len(aIntegr)
						If aIntegr[nD][1] == TMP->ZL_PLINTTP .And. aIntegr[nD][2] == TMP->ZL_PLINTOR .And. aIntegr[nD][3] == TMP->ZL_PLINTCD .And. aIntegr[nD][4] == TMP->ZL_PLINTLJ 
						
							aAdd(aIntegr[nD][6],{;
								cCod,;
								cDescr,;
								nValor,;
								nTxAdm,;
								nFundo,;
								nINSS,;
								nTotal;
							})
						
							laIntegr:= .T.
						EndIf
					Next nD
				EndIf
				
				// Se n�o existir o Integrante
				If !laIntegr
					// Adiciona o integrante
					aAdd(aIntegr,{;
						TMP->ZL_PLINTTP,;
						TMP->ZL_PLINTOR,;
						TMP->ZL_PLINTCD,;
						TMP->ZL_PLINTLJ,;
						AllTrim(TMP->ZL_PLINTNM),;
						{};
					})
					// Adiciona o adicional
					aAdd(aIntegr[Len(aIntegr)][6],{;
						cCod,;
						cDescr,;
						nValor,;
						nTxAdm,;
						nFundo,;
						nINSS,;
						nTotal;
					})
				EndIf
				
				// Se for cobran�a 24H
				If TMP->ZL_PLCOB24 == 'S'
					
					// Define descri��o e valores
					cDescr	:= AllTrim(TMP->ZL_ADDESCR) + 'INCLUS�O 24H - ' + cValToChar(TMP->ZL_PLDIA24) + ' DIAS.'
					nValor := (TMP->ZL_PLVL / 30) * TMP->ZL_PLDIA24
					nFundo := TMP->ZL_PLVLFUN 
					nTxAdm	:= TMP->ZL_PLVLTXA
					nINSS	:= nValor * (TMP->ZL_PLPINSS / 100)
					nTotal	:= nValor + nINSS + nTxAdm + nFundo
					
					// Verifica se j� existe este integrante no Array. Se sim, adiciona o plano, se n�o, adiciona o integrante e o Adicional
					laIntegr := .F.
					If Len(aIntegr) > 0
						For nD := 1 To Len(aIntegr)
							If aIntegr[nD][1] == TMP->ZL_PLINTTP .And. aIntegr[nD][2] == TMP->ZL_PLINTOR .And. aIntegr[nD][3] == TMP->ZL_PLINTCD .And. aIntegr[nD][4] == TMP->ZL_PLINTLJ 
							
								aAdd(aIntegr[nD][6],{;
									cCod,;
									cDescr,;
									nValor,;
									nTxAdm,;
									nFundo,;
									nINSS,;
									nTotal;
								})
							
								laIntegr:= .T.
							EndIf
						Next nD
					EndIf
					
					// Se n�o existir o integrante
					If !laIntegr
						// Adiciona integrante
						aAdd(aIntegr,{;
							TMP->ZL_PLINTTP,;
							TMP->ZL_PLINTOR,;
							TMP->ZL_PLINTCD,;
							TMP->ZL_PLINTLJ,;
							AllTrim(TMP->ZL_PLINTNM),;
							{};
						})
						// Adiciona o adicional
						aAdd(aIntegr[Len(aIntegr)][6],{;
							cCod,;
							cDescr,;
							nValor,;
							nTxAdm,;
							nFundo,;
							nINSS,;
							nTotal;
						})
					EndIf
				
				EndIf
			// Co-Participa��o
			Else
				
				// Define valores
				cCod	:= TMP->ZL_CPRECNO
				nValor	:= TMP->ZL_CPVLSER
				nTxAdm	:= 0
				nFundo	:= 0
				nINSS	:= nValor * (TMP->ZL_CPPINSS / 100)
				nTotal	:= nValor + nINSS + nTxAdm + nFundo
				
				// Verifica se existe o coparticipa��o para este benefici�rio
				lDadosCp := .F.
				If Len(aDadosCp) > 0
					For nC := 1 To Len(aDadosCp)
						// Se existir, adiciona a atual, somando seus valores aos totais
						If aDadosCp[nC][3] == TMP->ZL_CPBENCD
							aDadosCp[nC][6]  += nValor
							aDadosCp[nC][7]  += nTxAdm
							aDadosCp[nC][8]  += nFundo
							aDadosCp[nC][9]  += nINSS
							aDadosCp[nC][10] += nTotal
							
							// Adiciona o detalhe da co-participa��o
							aAdd(aDadosCp[nC][11],{;
								cCod,;
								TMP->ZL_CPATDDS,;
								TMP->ZL_CPATDDT,;
								TMP->ZL_CPATDTP,;
								TMP->ZL_CPPRNOM,;
								nValor,;
								nTxAdm,;
								nFundo,;
								nINSS,;
								nTotal;
							})
							
							lDadosCp := .T.
							
							Exit
						EndIf
					Next nC
				EndIf
				
				// Se n�o existir a co-participa��o para este benefici�rio
				If !lDadosCp

					// Adiciona co-participa��o para o benefici�rio
					aAdd(aDadosCp,{;
						'C',;	// CO-PARTICIPACAO
						NIL,;
						TMP->ZL_CPBENCD,;
						NIL,;
						AllTrim(TMP->ZL_CPBENNM),;
						nValor,;
						nTxAdm,;
						nFundo,;
						nINSS,;
						nTotal,;
						{};
					})
					// Adiciona os detalhes da co-participa��o do benefici�rio
					aAdd(aDadosCp[Len(aDadosCp)][11],{;
						cCod,;
						TMP->ZL_CPATDDS,;
						TMP->ZL_CPATDDT,;
						TMP->ZL_CPATDTP,;
						TMP->ZL_CPPRNOM,;
						nValor,;
						nTxAdm,;
						nFundo,;
						nINSS,;
						nTotal;
					})
				EndIf
			
			EndIf
			
			TMP->(dbSkip())		
		End
		
		// Atribui as co-participa��es aos integrantes do plano
		For nD:= 1 To Len(aDadosCP)
			
			lCpIntegr := .F.
			
			// Se encontrar o integrante
			For nE := 1 To Len(aIntegr)
				If aDadosCP[nD][5] $ aIntegr[nE][5]
					// Adiciona o total e os �tens de co-participa��o
					aAdd(aIntegr[nD][6],{;
						'C',;
						'CO-PARTICIPACAO',;
						aDadosCp[nD][6],;	// Valor
						aDadosCp[nD][7],;	// Tx Administrativa
						aDadosCp[nD][8],;	// Fundo
						aDadosCp[nD][9],;	// INSS
						aDadosCp[nD][10],;	// Valor Total
						aDadosCp[nD][11];	// �tens da Co-participa��o
					})
					
					lCpIntegr := .T.				
					Exit
					
				EndIf
			Next nE
			
			// SE n�o encontrar o integrante,
			If !lCpIntegr
				// Adiciona o integrante
				aAdd(aIntegr,{;
					'C',;
					NIL,;
					aDadosCp[nD][3],;
					NIL,;
					aDadosCp[nD][5],;
					{};
				})
				// Adiciona o total da co-participa��o e os detalhes
				aAdd(aIntegr[Len(aIntegr)][6],{;
					'C',;
					'CO-PARTICIPACAO',;
					aDadosCp[nD][6],;	// Valor
					aDadosCp[nD][7],;	// Tx Administrativa
					aDadosCp[nD][8],;	// Fundo
					aDadosCp[nD][9],;	// INSS
					aDadosCp[nD][10],;	// Valor Total
					aDadosCp[nD][11];	// �tens da Co-participa��o
				})			
			EndIf
		
		Next nD
		
		// Vefifica se j� existe a fatura no array de dados 
		lTitul := .F.
		If Len(aDados) > 0
			For nB := 1 To Len(aDados)
				// Se existir, adiciona o integrante e seus valores
				If aDados[nB][1] == aDadosTit[nA][9]
					aAdd(aDados[nB][2],{aDadosTit[nA], aIntegr})
					lTitul := .T.
				EndIf
			Next nB
		EndIf
		// Se n�o existir, adiciona a fatura, o integrante e seus valores
		If !lTitul
			aAdd(aDados,{aDadosTit[nA][9],{}})
			
			aAdd(aDados[Len(aDados)][2],{aDadosTit[nA], aIntegr})
		EndIf

		TMP->(dbCloseArea())		
	Next nA
	
	// Verifica se existem dados para o relat�rio. Se existir, executa o relat�rio
	// SE n�o existir avisa o usu�rio e finaliza
	If Len(aDados) > 0
		GerRelat(aDados)
	Else
		Alert("Nenhuma informa��o encontrada com os par�metro informados. Relat�rio n�o gerado.")
	EndIf	
 
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GERRELAT	! Autor !Gilson Lima 		 ! Data ! 02/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para gera��o do Relat�rio de Repasse das mensalidades 	 !
! 			! dos fiscais													 !
+----------------------------------------------------------------------------*/
Static Function GerRelat(aDados)

	Private	nLFinal	    := 3170
	
	Private oRel		:= Nil
	Private nLinha		:= 0
	Private nPage		:= 0

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

	oRel := TMSPrinter():New("Mensalidade Servi�os de Sa�de")
	oRel:Setup()
	oRel:SetPortrait()

	cTitulo   := "MENSALIDADE SERVI�OS DE SA�DE"	
	
	For Nx := 1 To Len(aDados)	// Cliente
	
		CabRelat(cTitulo,cCompRelat)
		
		nTitTVal	:= 0
		nTitTTxAdm	:= 0
		nTitTFundo	:= 0
		nTitTINSS	:= 0
		nTitTTot 	:= 0		
	
		For nA := 1 To Len(aDados[nX][2])	// Fatura
		
			oRel:Line(nLinha,0100,nLinha,2300)
		
			// Titular
			
			cFamilia	:= AllTrim(aDados[nX][2][nA][1][11])
			cTitNome 	:= AllTrim(POSICIONE ("SA1",1,xFilial("SA1")+aDados[nX][2][nA][1][9]+aDados[nX][2][nA][1][3],"A1_NOME"))
			
			nLinha += 70
			
			// Fam�lia - Titular
			oRel:Say(nLinha,0100,'FAM�LIA - TITULAR: ',oFont10b)
			oRel:Say(nLinha,0470,cFamilia+' - '+cTitNome,oFont10b)
		
			nFatTVal	:= 0
			nFatTTxAdm	:= 0
			nFatTFundo	:= 0
			nFatTINSS	:= 0
			nFatTTot 	:= 0
			
			// Integrante
			For nB := 1 To Len(aDados[nX][2][nA][2])
				
				// Nome
				cNomeIntegr	:= AllTrim(aDados[nX][2][nA][2][nB][5])
				
				// Data de Nascimento
				// ZJ_FILIAL + ZJ_CODIGO + ZJ_LOJA
				dDtNascInt	:= StoD('')
				If aDados[nX][2][nA][2][nB][4] != NIL
					dDtNascInt := POSICIONE("SZJ",4,xFilial("SZJ")+aDados[nX][2][nA][2][nB][3]+aDados[nX][2][nA][2][nB][4],"ZJ_DTNASC")
				EndIf
				
				nLinha += 150
				
				cIntNasc := cNomeIntegr

				// Imprime data de nascimento se estiver dispon�vel
				If AllTrim(DtoS(dDtNascInt)) != ''
					cIntNasc += " - Dt. Nasc.: " + DtoC(dDtNascInt)
				EndIf
				
				// Cabe�alho do Titular		
				oRel:Say(nLinha,0100,cIntNasc,oFont9b)
				
				// Cabe�alho de �tens
				CabecLin()
								
				nIntTVal	:= 0
				nIntTTxAdm	:= 0
				nIntTFundo	:= 0
				nIntTINSS	:= 0
				nIntTTot 	:= 0
				
				For nC := 1 To Len(aDados[nX][2][nA][2][nB][6])

					// Verifica fim da p�gina			
					If nLinha > nLFinal
						// Imprime Rodap�
						RodRelat()
					
						oRel:EndPage()
						CabRelat(cTitulo,cCompRelat)
						CabecLin()
					EndIf
					
					// Soma valores para exibir total por Integrante
					nIntTVal	+= aDados[nX][2][nA][2][nB][6][nC][3]
					nIntTTxAdm	+= aDados[nX][2][nA][2][nB][6][nC][4]
					nIntTFundo	+= aDados[nX][2][nA][2][nB][6][nC][5]
					nIntTINSS	+= aDados[nX][2][nA][2][nB][6][nC][6]
					nIntTTot 	+= aDados[nX][2][nA][2][nB][6][nC][7]
					
				
					// Tratamento dos valores
					cCod 	:= AllTrim(aDados[nX][2][nA][2][nB][6][nC][1])
					cDescr 	:= AllTrim(aDados[nX][2][nA][2][nB][6][nC][2])
					cValor 	:= Transform(aDados[nX][2][nA][2][nB][6][nC][3],"@E 999,999.99")
					cTxAdm 	:= Transform(aDados[nX][2][nA][2][nB][6][nC][4],"@E 999,999.99")
					cFundo 	:= Transform(aDados[nX][2][nA][2][nB][6][nC][5],"@E 999,999.99")
					cINSS 	:= Transform(aDados[nX][2][nA][2][nB][6][nC][6],"@E 999,999.99")
					cTotal 	:= Transform(aDados[nX][2][nA][2][nB][6][nC][7],"@E 999,999.99")
					
					// Impress�o dos �tens
					nLinha += 50
					oRel:Say(nLinha,0100,cCod,oFont9)
					oRel:Say(nLinha,0250,cDescr,oFont9)
					oRel:Say(nLinha,1100,cValor,oFont9m)
					oRel:Say(nLinha,1350,cTxAdm,oFont9m)
					oRel:Say(nLinha,1600,cFundo,oFont9m)
					oRel:Say(nLinha,1850,cINSS,oFont9m)
					oRel:Say(nLinha,2100,cTotal,oFont9m)
				
				Next nC
				
				// Verifica fim da p�gina			
				If nLinha + 100 > nLFinal
					// Imprime Rodap�
					RodRelat()
				
					oRel:EndPage()
					CabRelat(cTitulo,cCompRelat)
					CabecLin()
				EndIf
				
				// Impress�o do total por integrante
				nLinha += 50

				oRel:Say(nLinha,1100,Replicate('-',60),oFont9m)

				nLinha += 50
				
				oRel:Say(nLinha,0600,"TOTAL INTEGRANTE:",oFont9b)
				oRel:Say(nLinha,1100,Transform(nIntTVal,"@E 999,999.99"),oFont9mb)
				oRel:Say(nLinha,1350,Transform(nIntTTxAdm,"@E 999,999.99"),oFont9mb)
				oRel:Say(nLinha,1600,Transform(nIntTFundo,"@E 999,999.99"),oFont9mb)
				oRel:Say(nLinha,1850,Transform(nIntTINSS,"@E 999,999.99"),oFont9mb)
				oRel:Say(nLinha,2100,Transform(nIntTTot,"@E 999,999.99"),oFont9mb)
				
				// Acrescenta valores ao total da Fatura
				
				nFatTVal	+= nIntTVal
				nFatTTxAdm	+= nIntTTxAdm
				nFatTFundo	+= nIntTFundo
				nFatTINSS	+= nIntTINSS
				nFatTTot 	+= nIntTTot
				
				// Acrescentar Co-Participa��o
				If lDetCoPart
					
					lImpCabCoP := .F.
					
					For nC := 1 To Len(aDados[nX][2][nA][2][nB][6])
						
						If AllTrim(aDados[nX][2][nA][2][nB][6][nC][1]) == 'C'
						
							For nD := 1 To Len(aDados[nX][2][nA][2][nB][6][nC][8])
								
								// Verifica fim da p�gina			
								If nLinha + 120 > nLFinal
									// Imprime Rodap�
									RodRelat()
								
									oRel:EndPage()
									CabRelat(cTitulo,cCompRelat)
								EndIf
								
								// Imprime cabe�alho de Co-Participa��o
								If lImpCabCoP == .F.
								
									nLinha += 70
									
									oRel:Say(nLinha,0250,'�TENS DE CO-PARTICIPA��O:',oFont9b)
									
									lImpCabCoP := .T.
								EndIf
								
								cDataProc := DtoC(StoD(aDados[nX][2][nA][2][nB][6][nC][8][nD][3])) 					 	// Data do Procedimento
								cTipoProc := AllTrim(aDados[nX][2][nA][2][nB][6][nC][8][nD][4])					 	// Tipo do Procedimento
								cDescProc := AllTrim(aDados[nX][2][nA][2][nB][6][nC][8][nD][2])					 	// Descri��o do Procedimento
								cProfProc := AllTrim(aDados[nX][2][nA][2][nB][6][nC][8][nD][5])					 	// Local do Procedimento
								cValProc  := Transform(aDados[nX][2][nA][2][nB][6][nC][8][nD][6],  "@E 999,999.99") // Valor do Procedimento
								cTxAdProc := Transform(aDados[nX][2][nA][2][nB][6][nC][8][nD][7],  "@E 999,999.99") // Tx. Adm. 
								cFunProc  := Transform(aDados[nX][2][nA][2][nB][6][nC][8][nD][8],  "@E 999,999.99") // Fundo
								cINSSProc := Transform(aDados[nX][2][nA][2][nB][6][nC][8][nD][9],  "@E 999,999.99") // INSS
								cTotProc  := Transform(aDados[nX][2][nA][2][nB][6][nC][8][nD][10], "@E 999,999.99") // Total
								
								cDescrProc := cDataProc + " - " + cTipoProc + ' - ' + cDescProc
								
								// Impress�o dos �tens de Co-Participa��o
								nLinha += 50
								
								// Data - Tipo - Descr - Profissional
								oRel:Say(nLinha,0250,cDescrProc,oFont9)
								//oRel:Say(nLinha,0250,cDescr,oFont9)
								//oRel:Say(nLinha,1100,cValProc,oFont9m)
								//oRel:Say(nLinha,1350,cTxAdProc,oFont9m)
								//oRel:Say(nLinha,1600,cFunProc,oFont9m)
								//oRel:Say(nLinha,1850,cINSSProc,oFont9m)
								oRel:Say(nLinha,2100,cTotProc,oFont9m)
								
							Next nD
						
						EndIf
				
					Next nC				
				EndIf				
									
			Next nB

			// Verifica fim da p�gina			
			If nLinha + 200 > nLFinal
				// Imprime Rodap�
				RodRelat()
			
				oRel:EndPage()
				CabRelat(cTitulo,cCompRelat)
				CabecLin()
			EndIf

				
			// Impress�o do total da Fatura/Servi�o
			nLinha += 150

			oRel:Line(nLinha,1100,nLinha,2300)

			nLinha += 50
			
			If aDados[nX][2][nA][1][1] == 'PLO'
				cDescServ := 'ODONTO'
			Else
				cDescServ := 'SA�DE'
			EndIf
			
			oRel:Say(nLinha,0600,"TOTAL "+cDescServ+" :",oFont9b)
			oRel:Say(nLinha,1100,Transform(nFatTVal,"@E 999,999.99"),oFont9mb)
			oRel:Say(nLinha,1350,Transform(nFatTTxAdm,"@E 999,999.99"),oFont9mb)
			oRel:Say(nLinha,1600,Transform(nFatTFundo,"@E 999,999.99"),oFont9mb)
			oRel:Say(nLinha,1850,Transform(nFatTINSS,"@E 999,999.99"),oFont9mb)
			oRel:Say(nLinha,2100,Transform(nFatTTot,"@E 999,999.99"),oFont9mb)		
			
			// Acrescenta valores ao total da Titular
			
			nTitTVal	+= nFatTVal
			nTitTTxAdm	+= nFatTTxAdm
			nTitTFundo	+= nFatTFundo
			nTitTINSS	+= nFatTINSS
			nTitTTot 	+= nFatTTot
				
			
		Next nA
		
		// Verifica fim da p�gina			
		If nLinha + 200 > nLFinal
			// Imprime Rodap�
			RodRelat()
		
			oRel:EndPage()
			CabRelat(cTitulo,cCompRelat)
			CabecLin()
		EndIf
		
				
		// Impress�o do total do Titular
		nLinha += 150

		oRel:Line(nLinha,0100,nLinha,2300)

		nLinha += 50
		
		oRel:Say(nLinha,0600,"TOTAL GERAL:",oFont9b)
		oRel:Say(nLinha,1100,Transform(nTitTVal,"@E 999,999.99"),oFont9mb)
		oRel:Say(nLinha,1350,Transform(nTitTTxAdm,"@E 999,999.99"),oFont9mb)
		oRel:Say(nLinha,1600,Transform(nTitTFundo,"@E 999,999.99"),oFont9mb)
		oRel:Say(nLinha,1850,Transform(nTitTINSS,"@E 999,999.99"),oFont9mb)
		oRel:Say(nLinha,2100,Transform(nTitTTot,"@E 999,999.99"),oFont9mb)
		
		// Imprime Rodap�
		RodRelat()
					
		oRel:EndPage()		
		
	Next nX					
	
	oRel:Preview()	

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! CABRELAT	! Autor !Gilson Lima 		 ! Data ! 24/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Imprime Cabe�alho do Relat�rio								 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function CabRelat(cTitulo,cCompRelat)

	Local cLogo :='\system\sindafep.bmp'
	
	nPage++
	
	oRel:StartPage()

	cMens := "M�s/Ano de Refer�ncia: "+cCompRelat

	nLinha := 0

	If (File(cLogo))
		oRel:SayBitmap(nLinha+35,100,cLogo,310,200)
	EndIf
	
	nLinha += 50

	oRel:Say(nLinha,900,cTitulo,oFont14)

	nLinha += 80
	oRel:Say(nLinha,1080,cMens,oFont10b)
	
	nLinha += 150

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RODRELAT	! Autor !Gilson Lima 		 ! Data ! 24/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Imprime Rodap� do Relat�rio								 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function RodRelat()

	oRel:Line(nLFinal + 20,0100,nLFinal + 20,2300)
	oRel:Say(nLFinal + 40,0100,'SINDAFEP- Sindicato dos Auditores Fiscais da Receita do Estado do Paran�',oFont9)
	oRel:Say(nLFinal + 40,1800,DtoC(dDataBase) + '  ' + Time() + '  P�gina: ' + cValToChar(nPage),oFont9)

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! CABCLIN	! Autor !Gilson Lima 		 ! Data ! 24/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Imprime Cabe�alho de �tens									 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function CabecLin()

	nLinha += 80
	oRel:Say(nLinha,0100,"Cod.",oFont9b)
	oRel:Say(nLinha,0250,"Descri��o",oFont9b)
	oRel:Say(nLinha,1100,Replicate(' ',10)+"Valor",oFont9b)
	oRel:Say(nLinha,1350,Replicate(' ',4)+"Tx. Adm.",oFont9b)
	oRel:Say(nLinha,1600,Replicate(' ',8)+"Fundo",oFont9b)
	oRel:Say(nLinha,1850,Replicate(' ',10)+"INSS",oFont9b)
	oRel:Say(nLinha,2100,Replicate(' ',8)+"TOTAL",oFont9b)
	
	nLinha += 20
Return