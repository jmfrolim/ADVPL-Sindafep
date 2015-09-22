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
!Nome 			   ! REL-INFORMEIRPF										 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Relatório de Informações IRPF							 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 27/02/2015												 !
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
! Programa 	! GRELIRPF	! Autor !Gilson Lima 		 ! Data ! 27/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Tela da Rotina do Relatório de Informe IRPF					 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function GRelIRPF()

	Private cPerg		:= "GRELIRPF"
	Private aPergs		:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		// Competência
		AADD(aPergs,{"Ano Exercício:","","","mv_ch1","C",4,0,0,"G","NAOVAZIO()","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 9999"})
		// Titulares
		AADD(aPergs,{"Todos os Titulares?","","","mv_ch2","C",1,0,0,"C","","MV_PAR02","Sim","","","","","Não","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Selecione o Titular:","","","mv_ch3","C",99,0,0,"G","","MV_PAR03","","","","000001","","","","","","","","","","","","","","","","","","","","","SA1PLN","","","",""})
		// Planos
		AADD(aPergs,{"Todos os Planos?","","","mv_ch4","C",1,0,0,"C","","MV_PAR04","Sim","","","","","Não","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Selecione o Plano:","","","mv_ch5","C",99,0,0,"G","","MV_PAR05","","","","000001","","","","","","","","","","","","","","","","","","","","","SZDGRP","","","",""})
		
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	If (!Pergunte(cPerg,.T.))
		Return
	EndIf

	//Chama função geração de títulos
	If MsgYesNo("Confirma a Geração do Relatório?","Atenção")
		Processa({||U_GrRelIRPF()}) 
	Endif 
	
Return()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GrRelIRPF	! Autor !Gilson Lima 		 ! Data ! 27/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para geração do Relatório de Informe IRPF			 	 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function GrRelIRPF()

	Local	cQuery		:= ''
	Local	aDadosTit	:= {}
	Local	aDadosTit1	:= {}
	Local	aDadosPl	:= {}
	Local	aDados		:= {}
	Local	cFormCobr	:= ''
	Local	cTitPref	:= ''
		
	Private cAnoExerc	:= MV_PAR01	// Ano Exercício
	Private	lTTitular	:= IIF(MV_PAR02 == 1, .T., .F.)
	Private aTitular	:= StrToKarr(AllTrim(MV_PAR03),'|')
	Private lTPlano		:= IIF(MV_PAR04 == 1, .T., .F.)
	Private cPlano		:= AllTrim(MV_PAR05)
	
	Private dIniExerc	:= CtoD('01/01/'+cAnoExerc)	// Primeiro dia do ano
	Private dFimExerc	:= CtoD('31/12/'+cAnoExerc)	// Último dia do ano
	
	cQuery	:= "SELECT SE5.E5_FILIAL, SE5.E5_PREFIXO, SE5.E5_NUMERO, SE5.E5_LOJA, SE5.E5_DATA, SE5.E5_CLIFOR"
	cQuery  += " FROM " + RetSqlName("SE5") + " SE5"
	cQuery	+= " WHERE SE5.D_E_L_E_T_ <> '*'"
	cQuery	+= " AND SE5.E5_DATA >= '" + DtoS(dIniExerc) + "'"
	cQuery	+= " AND SE5.E5_DATA <= '" + DtoS(dFimExerc) + "'"
	cQuery	+= " AND SE5.E5_MOTBX <> 'FAT'"	// MOTIVO DE BAIXA DIFERENTE DE AGLUTINADO
	
	// Titular
	If !lTTitular
		cQuery += " AND SE5.E5_CLIFOR = '" + aTitular[1] + "'"	// Código Cliente
		cQuery += " AND SE5.E5_LOJA = '" + aTitular[2] + "'"	// Loja
	EndIf
	
	// Grupo de Planos
	If !lTPlano
		Do Case
			Case cPlano == '000001'
				cTitPref := 'PLS'
			Case cPlano	== '000002'
				cTitPref := 'PLO'
		EndCase
		
		// Retorna Tipo de plano selecionado + PLN para posterior verificação
		cQuery += " AND (SE5.E5_PREFIXO = '" + cTitPref + "' OR SE5.E5_PREFIXO = 'PLN')"
	Else
		// Retorna todos os tipos de saúde + PLN para posterior verificação
		cQuery += " AND (SE5.E5_PREFIXO = 'PLS' OR SE5.E5_PREFIXO = 'PLO' OR SE5.E5_PREFIXO = 'PLN')"
	EndIf
	
	// Ordenação do resultado
	cQuery	+= " ORDER BY SE5.E5_LOJA, SE5.E5_CLIFOR, SE5.E5_DATA, SE5.E5_NUMERO"

	If Select('TMP') <> 0
		dbSelectArea('TMP')
		TMP->(dbCloseArea())
	EndIf
	
	dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TMP',.F.,.F.)
	
	dbSelectArea('TMP')
	TMP->(dbGoTop())

	While TMP->(!EOF())
		
		// Adiciona ao Array de Dados do Título
		aAdd(aDadosTit1,{;
			TMP->E5_PREFIXO,;
			TMP->E5_NUMERO,;
			TMP->E5_LOJA,;
			TMP->E5_DATA,;
			TMP->E5_FILIAL,;
			TMP->E5_CLIFOR;
		})
		
		TMP->(dbSkip())		
	End
	
	TMP->(dbCloseArea())
	
	// Se não houver dados para gerar o relatório, informa e retorna
	If Len(aDadosTit1) == 0	
		Alert("Nenhuma informação encontrada com os parâmetro informados. Relatório não gerado.")
		Return
	EndIf
	
	For nT := 1 To Len(aDadosTit1)
	
		// SE for fatura aglutinada, busca títulos de origem para o adicionar ao array de trabalho
		If aDadosTit1[nT][1] == 'PLN'
		
			cQuery 	:= "SELECT SE5.E5_FILIAL, SE5.E5_PREFIXO, SE5.E5_NUMERO, SE5.E5_LOJA, SE5.E5_DATA, SE5.E5_CLIFOR"
			cQuery 	+= " FROM " + RetSqlName("SE5") + " SE5"
			cQuery	+= " WHERE SE5.D_E_L_E_T_ <> '*'"
			cQuery  += " AND SE5.E5_FATURA = '" + aDadosTit1[nT][2] + "'"
			cQuery  += " AND SE5.E5_FATPREF = '" + aDadosTit1[nT][1] + "'"

			// Grupo de Planos
			If !lTPlano

				// Retorna Tipo de plano selecionado
				cQuery += " AND SE5.E5_PREFIXO = '" + cTitPref + "'"
			Else
				// Retorna todos os tipos de Planos
				cQuery += " AND (SE5.E5_PREFIXO = 'PLS' OR SE5.E5_PREFIXO = 'PLO')"
			EndIf
			
			If Select('TMP') <> 0
				dbSelectArea('TMP')
				TMP->(dbCloseArea())
			EndIf
			
			dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TMP',.F.,.F.)
			
			dbSelectArea('TMP')
			TMP->(dbGoTop())
		
			While TMP->(!EOF())
				
				// Adiciona ao Array de Dados do Título
				aAdd(aDadosTit,{;
					TMP->E5_PREFIXO,;
					TMP->E5_NUMERO,;
					TMP->E5_LOJA,;
					aDadosTit1[nT][4],;	// Data da Fatura = Data de pagamento da fatura Aglutinada
					TMP->E5_FILIAL,;
					TMP->E5_CLIFOR;
				})
				
				TMP->(dbSkip())		
			End
			
			TMP->(dbCloseArea())
			
		// Se não for Aglutinada transporta para o Array de Trabalho
		Else
			aAdd(aDadosTit,aDadosTit1[nT])
		EndIf
	
	Next nT
	
	// Grava Array com informações dos Títulos
	For nA := 1 To Len(aDadosTit)
		// Busca dados dos planos e grava em array
		
		cQuery := "SELECT SZL.ZL_PLINTTP, SZL.ZL_PLINTOR, SZL.ZL_PLINTCD, SZL.ZL_PLINTLJ, SZL.ZL_PLINTNM, SZL.ZL_PLGRPCD, SZL.ZL_PLCOD, SZL.ZL_PLDESCR,"
		cQuery += " SZL.ZL_PLFXDSC, SZL.ZL_PLVL, SZL.ZL_PLVLCOB, SZL.ZL_PLVLFUN, SZL.ZL_PLVLTXA, SZL.ZL_PLPINSS, SZL.ZL_PLPFUNC,"
		cQuery += " SZL.ZL_PLINC24, SZL.ZL_PLCOB24, SZL.ZL_PLDIA24, SZL.ZL_PLVL24, SZL.ZL_ADCOD, SZL.ZL_ADDESCR, SZL.ZL_CPRECNO, SZL.ZL_CPBENCD, SZL.ZL_CPBENNM,"
		cQuery += " SZL.ZL_CPATDDT, SZL.ZL_CPATDTP, SZL.ZL_CPATDDS, SZL.ZL_CPPRNOM, SZL.ZL_CPVLSER, SZL.ZL_CPCOBDV, SZL.ZL_CPPINSS"
		cQuery += " FROM " + RetSqlName("SZL") + " SZL"
		cQuery += " WHERE SZL.D_E_L_E_T_ <> '*'"
		cQuery += " AND SZL.ZL_TPDADO <> 'F'"
		cQuery += " AND SZL.ZL_ADDIRPF = 'S'"	// Retorna só os ítens que declaram IRPF
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
		
			cMes	:= Substr(aDadosTit[nA][4],5,2) 
			
			// Se for plano de Saúde
			If AllTrim(TMP->ZL_PLCOD) != ''
			
				// Calcula valores
				nValor	:= TMP->ZL_PLVL
				nFundo	:= TMP->ZL_PLVLFUN
				
				// Se for titular e funcionário
				If TMP->ZL_PLINTTP == 'T' .And. TMP->ZL_PLINTOR == '000003'	
					nDesFunc := TMP->ZL_PLPFUNC
				Else
					nDesFunc := 0
				EndIf

				nTotal	:= nValor + nFundo - nDesFunc
				
				
				// Verifica se já existe este integrante no Array. Se sim, adiciona o plano, se não, adiciona o integrante e o Plano
				laIntegr := .F.
				If Len(aIntegr) > 0
					For nD := 1 To Len(aIntegr)
						If aIntegr[nD][1] == TMP->ZL_PLINTTP .And. aIntegr[nD][2] == TMP->ZL_PLINTOR .And. aIntegr[nD][3] == TMP->ZL_PLINTCD .And. aIntegr[nD][4] == TMP->ZL_PLINTLJ 
						
							// Adiciona Valor ao mês correspondente
							aIntegr[Len(aIntegr)][6][val(cMes)] += nTotal
						
							laIntegr:= .T.
						EndIf
					Next nD
				EndIf
				
				// Se não existir o integrante
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
					// Adiciona Meses
					For nH := 1 To 12
						aAdd(aIntegr[Len(aIntegr)][6],0)
					Next nH
					
					// Adiciona Valor ao mês correspondente
					aIntegr[Len(aIntegr)][6][val(cMes)] += nTotal
				EndIf
				
				// Se for cobrança 24H
				If TMP->ZL_PLCOB24 == 'S'
					
					// Calcula valores
					nValor := TMP->ZL_PLVL
					nValor := (nValor / 30) * TMP->ZL_PLDIA24
					nFundo := TMP->ZL_PLVLFUN 
					
					// Se for titular e funcionário executa o referido desconto
					If TMP->ZL_PLINTTP == 'T' .And. TMP->ZL_PLINTOR == '000003'	// Se for titular e funcionário
						nDesFunc := (TMP->ZL_PLPFUNC / 30) * TMP->ZL_PLDIA24
					Else
						nDesfunc := 0
					EndIf
					
					nTotal	:= nValor + nFundo - nDesFunc
	
					// // Verifica se já existe este integrante no Array. Se sim, adiciona o valor no mês correpondente, se não, adiciona o integrante e os valores			
					laIntegr := .F.
					If Len(aIntegr) > 0
						For nD := 1 To Len(aIntegr)
							If aIntegr[nD][1] == TMP->ZL_PLINTTP .And. aIntegr[nD][2] == TMP->ZL_PLINTOR .And. aIntegr[nD][3] == TMP->ZL_PLINTCD .And. aIntegr[nD][4] == TMP->ZL_PLINTLJ 
							
								// Adiciona Valor ao mês correspondente
								aIntegr[Len(aIntegr)][6][val(cMes)] += nTotal
							
								laIntegr:= .T.
							EndIf
						Next nD
					EndIf
					
					// Se não existir integrante
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
						// Adiciona Meses
						For nH := 1 To 12
							aAdd(aIntegr[Len(aIntegr)][6],0)
						Next nH
						
						// Adiciona Valor ao mês correspondente
						aIntegr[Len(aIntegr)][6][val(cMes)] += nTotal
					EndIf
				
				EndIf
			
			// Se for Adicional
			ElseIf AllTrim(TMP->ZL_ADCOD) != ''
				
				// Calcula valores
				nValor	:= TMP->ZL_PLVL
				nFundo	:= TMP->ZL_PLVLFUN
				
				// Se for titular e funcionário
				If TMP->ZL_PLINTTP == 'T' .And. TMP->ZL_PLINTOR == '000003'	
					nDesFunc := TMP->ZL_PLPFUNC
				Else
					nDesFunc := 0
				EndIf
				
				nTotal	:= nValor + nFundo - nDesFunc
				
				// Verifica se já existe este integrante no Array. Se sim, adiciona o valor no mês correpondente, se não, adiciona o integrante e os valores
				laIntegr := .F.
				If Len(aIntegr) > 0
					For nD := 1 To Len(aIntegr)
						If aIntegr[nD][1] == TMP->ZL_PLINTTP .And. aIntegr[nD][2] == TMP->ZL_PLINTOR .And. aIntegr[nD][3] == TMP->ZL_PLINTCD .And. aIntegr[nD][4] == TMP->ZL_PLINTLJ 
						
							// Adiciona Valor ao mês correspondente
							aIntegr[Len(aIntegr)][6][val(cMes)] += nTotal
						
							laIntegr:= .T.
						EndIf
					Next nD
				EndIf
				
				// Se não existir o Integrante
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
					// Adiciona Meses
					For nH := 1 To 12
						aAdd(aIntegr[Len(aIntegr)][6],0)
					Next nH
					
					// Adiciona Valor ao mês correspondente
					aIntegr[Len(aIntegr)][6][val(cMes)] += nTotal
				EndIf
				
				// Se for cobrança 24H
				If TMP->ZL_PLCOB24 == 'S'
					
					// Define valores
					nValor := (TMP->ZL_PLVL / 30) * TMP->ZL_PLDIA24
					nFundo := TMP->ZL_PLVLFUN 

					// Se for titular e funcionário
					If TMP->ZL_PLINTTP == 'T' .And. TMP->ZL_PLINTOR == '000003'	
						nDesFunc := (TMP->ZL_PLPFUNC / 30) * TMP->ZL_PLDIA24
					Else
						nDesFunc := 0
					EndIf
									
					nTotal	:= nValor + nFundo - nDesFunc
					
					// Verifica se já existe este integrante no Array. Se sim, adiciona o valor no mês correpondente, se não, adiciona o integrante e os valores
					laIntegr := .F.
					If Len(aIntegr) > 0
						For nD := 1 To Len(aIntegr)
							If aIntegr[nD][1] == TMP->ZL_PLINTTP .And. aIntegr[nD][2] == TMP->ZL_PLINTOR .And. aIntegr[nD][3] == TMP->ZL_PLINTCD .And. aIntegr[nD][4] == TMP->ZL_PLINTLJ 
							
								// Adiciona Valor ao mês correspondente
								aIntegr[Len(aIntegr)][6][val(cMes)] += nTotal
							
								laIntegr:= .T.
							EndIf
						Next nD
					EndIf
					
					// Se não existir o integrante
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
						// Adiciona Meses
						For nH := 1 To 12
							aAdd(aIntegr[Len(aIntegr)][6],0)
						Next nH
						
						// Adiciona Valor ao mês correspondente
						aIntegr[Len(aIntegr)][6][val(cMes)] += nTotal
					EndIf
				
				EndIf
			// Co-Participação
			Else
				
				// Define valores
				nValor	:= TMP->ZL_CPVLSER
				nFundo	:= 0
				nTotal	:= nValor + nFundo
				
				// Verifica se existe o coparticipação para este beneficiário
				lDadosCp := .F.
				If Len(aDadosCp) > 0
					For nC := 1 To Len(aDadosCp)
						// Se existir, adiciona a atual, somando seus valores aos totais
						If aDadosCp[nC][3] == TMP->ZL_CPBENCD

							aDadosCp[nC][6] += nTotal
							lDadosCp := .T.
							
							Exit
						EndIf
					Next nC
				EndIf
				
				// Se não existir a co-participação para este beneficiário
				If !lDadosCp

					// Adiciona co-participação para o beneficiário
					aAdd(aDadosCp,{;
						'C',;	// CO-PARTICIPACAO
						NIL,;
						TMP->ZL_CPBENCD,;
						NIL,;
						AllTrim(TMP->ZL_CPBENNM),;
						nTotal;
					})

				EndIf
			
			EndIf
			
			TMP->(dbSkip())		
		End
		
		// Atribui as co-participações aos integrantes do plano
		For nD:= 1 To Len(aDadosCP)
			
			lCpIntegr := .F.
			
			// Se encontrar o integrante
			For nE := 1 To Len(aIntegr)
				If Substr(aDadosCP[nD][5],1,10) $ Substr(aIntegr[nE][5],1,10)
					
					// Adiciona Valor ao mês correspondente
					aIntegr[Len(aIntegr)][6][val(cMes)] += aDadosCp[nD][6]

					lCpIntegr := .T.				
					Exit
					
				EndIf
			Next nE
			
			// SE não encontrar o integrante,
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
				
				// Adiciona Meses
				For nH := 1 To 12
					aAdd(aIntegr[Len(aIntegr)][6],0)
				Next nH
								
				// Adiciona Valor ao mês correspondente
				aIntegr[Len(aIntegr)][6][val(cMes)] += aDadosCp[nD][6]
						
			EndIf
		
		Next nD
		
		// Vefifica se já existe o titular no array de dados 
		lTitul := .F.
		If Len(aDados) > 0
			For nB := 1 To Len(aDados)
				// Se existir, adiciona o integrante e seus valores
				// COD TIT + LOJA + PREFIXO
				If aDados[nB][1] == aDadosTit[nA][6] .And. aDados[nB][2] == aDadosTit[nA][3] .And. aDados[nB][3] == aDadosTit[nA][1]
					
					// Verifica se o Integrante já está inserido neste Titular. Se Sim, soma os valores
					For nJ := 1 To Len(aIntegr)
						
						lInt := .F.
						For nK := 1 To Len(aDados[nB][4])
							If aIntegr[nJ][1] == aDados[nB][4][nK][1];
								.And. aIntegr[nJ][2] == aDados[nB][4][nK][2];
								.And. aIntegr[nJ][3] == aDados[nB][4][nK][3];
								.And. aIntegr[nJ][4] == aDados[nB][4][nK][4]

								For nW := 1 To Len(aDados[nB][4][nK][6])
									
									aDados[nB][4][nK][6][nW] += aIntegr[nJ][6][nW]
									
								Next nW
								
								lInt := .T.
								
							EndIf
						Next nK
						
						// Se não houver o integrante, adiciona
						If !Int
						
							aAdd(aDados[nB][4], aIntegr[nJ])
						
						EndIf
					
					Next nJ

					lTitul := .T.
				EndIf
			Next nB
		EndIf
		// Se não existir, adiciona o titular, o integrante e seus valores
		If !lTitul
			aAdd(aDados,{aDadosTit[nA][6],aDadosTit[nA][3],aDadosTit[nA][1],{}})
			
			aDados[Len(aDados)][4] := aIntegr
		EndIf

		TMP->(dbCloseArea())		
	Next nA
	
	// Verifica se existem dados para o relatório. Se existir, executa o relatório
	// SE não existir avisa o usuário e finaliza
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

	Private	nLFinal	    := 2500
	
	Private oRel		:= Nil
	Private nLinha		:= 0
	Private nPage		:= 0

	Private oFont9		:= TFont():New("Arial",9,9,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont9b		:= TFont():New("Arial",9,9,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont9m		:= TFont():New("Courier New",9,9,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont9mb	:= TFont():New("Courier New",9,9,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont10		:= TFont():New("Arial",10,10,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont10b	:= TFont():New("Arial",10,10,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont10m	:= TFont():New("Courier New",10,10,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont10mb	:= TFont():New("Courier New",10,10,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont12b	:= TFont():New("Arial",12,12,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont12		:= TFont():New("Arial",12,12,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont14		:= TFont():New("Arial",14,14,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont18		:= TFont():New("Arial",18,18,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont24		:= TFont():New("Arial",24,24,.T.,.T.,5,.T.,5,.T.,.F.)

	oRel := TMSPrinter():New("Declaração IRFP")
	oRel:Setup()
	oRel:SetLandscape()

	cTitulo   := "DECLARAÇÃO"	
	
	For Nx := 1 To Len(aDados)	// Titular

		CabRelat(cTitulo)
	
		// Dados Empresa Plano
		If aDados[nX][3] == 'PLO'		// Odontológico
			cEmpRazSoc	:= 'PLANO ODONTOLÓGICO'
			cEmpCNPJ	:= '99.999.999/9999-99'
		ElseIf aDados[nX][3] == 'PLS'	// Saúde
			cEmpRazSoc	:= 'UNIMED CURITIBA SOCIEDADE COOPERATIVA DE MÉDICOS'
			cEmpCNPJ	:= '75.055.772/0001-20'
		EndIf
	
		// Buscar Dados Titular
		cNomeTit := Upper(AllTrim(POSICIONE("SA1",1,xFilial("SA1")+aDados[nX][1]+aDados[nX][2],"A1_NOME")))
		cCPFTit	 := AllTrim(POSICIONE("SA1",1,xFilial("SA1")+aDados[nX][1]+aDados[nX][2],"A1_CGC"))
		
		nTotTit	 := {0,0,0,0,0,0,0,0,0,0,0,0}

		// Declaração
		cDeclara1	:= "DECLARAMOS que o senhor(a) " + cNomeTit + " portador do C.P.F. " + cCPFTit + ","
		cDeclara2	:= "inscrito(a) no Plano de Saúde " + cEmpRazSoc + ","
		cDeclara3	:= "inscrita no CNPJ " + cEmpCNPJ + ", em convênio com o SINDAFEP - Sindicato dos Auditores Fiscais da"
		cDeclara4	:= "Receita do Estado do Paraná, inscrito no CNPJ 76.707.686/0001-17, efetuou os pagamentos para o referido"
		cDeclara5	:= "Plano de Saúde no Exercício de " + cAnoExerc + ", conforme abaixo:"
		
		// Observação
		cObserva1	:= "***Atenção***"
		cObserva2	:= "A diferença entre os valores pagos e os valores aqui informados refere-se a Tarifa Bancária, INSS que não entra nas informações consolidadas!"
				
		// Imprimir a declaração
		
		oRel:Say(nLinha,0750,cDeclara1,oFont12)
		nLinha += 70
		oRel:Say(nLinha,0750,cDeclara2,oFont12)
		nLinha += 70
		oRel:Say(nLinha,0750,cDeclara3,oFont12)
		nLinha += 70
		oRel:Say(nLinha,0750,cDeclara4,oFont12)
		nLinha += 70
		oRel:Say(nLinha,0750,cDeclara5,oFont12)
		
		// Imprime Cabeçalho de Ítens		
		nLinha += 300
		
		oRel:Say(nLinha,0100,'Usuário',oFont9b)
		oRel:Say(nLinha,0650,'CPF',oFont9b)
		oRel:Say(nLinha,0850,Replicate(' ',6)+'Janeiro',oFont9b)
		oRel:Say(nLinha,1050,Replicate(' ',4)+'Fevereiro',oFont9b)
		oRel:Say(nLinha,1250,Replicate(' ',9)+'Março',oFont9b)
		oRel:Say(nLinha,1450,Replicate(' ',10)+'Abril',oFont9b)
		oRel:Say(nLinha,1650,Replicate(' ',10)+'Maio',oFont9b)
		oRel:Say(nLinha,1850,Replicate(' ',8)+'Junho',oFont9b)
		oRel:Say(nLinha,2050,Replicate(' ',9)+'Julho',oFont9b)
		oRel:Say(nLinha,2250,Replicate(' ',7)+'Agosto',oFont9b)
		oRel:Say(nLinha,2450,Replicate(' ',4)+'Setembro',oFont9b)
		oRel:Say(nLinha,2650,Replicate(' ',5)+'Outubro',oFont9b)
		oRel:Say(nLinha,2850,Replicate(' ',3)+'Novembro',oFont9b)
		oRel:Say(nLinha,3050,Replicate(' ',3)+'Dezembro',oFont9b)
		oRel:Say(nLinha,3250,Replicate(' ',10)+'Total',oFont9b)
		
		nLinha += 60
		
		oRel:Line(nLinha,0100,nLinha,3450)
		
		// Imprime integrantes e valores
		For nB := 1 To Len(aDados[nX][4])
			
			// Busca dados
			If aDados[nX][4][nB][1] == 'T'
				cNomeInt	:= cNomeTit
				cCPFInt		:= cCPFTit
			ElseIf aDados[nX][4][nB][1] == 'D'
				cNomeInt	:= UPPER(AllTrim(POSICIONE("SZG",1,xFilial("SZG")+aDados[nX][4][nB][3],"ZG_NOME")))
				cCPFInt		:= AllTrim(POSICIONE("SZG",1,xFilial("SZG")+aDados[nX][4][nB][3],"ZG_CPF"))
			Else
				cNomeInt	:= UPPER(AllTrim(aDados[nX][4][nB][5]))
				cCPFInt		:= AllTrim(POSICIONE("SZ1",3,xFilial("SZ1")+aDados[nX][4][nB][3],"Z1_CPF"))
			EndIf
			
			nLinha += 60		
			
			oRel:Say(nLinha,0100,Substr(cNomeInt,1,25),oFont9)
			oRel:Say(nLinha,0650,cCPFInt,oFont9)
			
			// Total do integrante
			nTotInt := 0
			nCol	:= 650
			
			For nC := 1 To 12
				
				nTotInt 	+= aDados[nX][4][nB][6][nC]	// Total do Integrante
				nTotTit[nC] += aDados[nX][4][nB][6][nC] // Total Mensal do Titular
				
				nCol += 200
				
				oRel:Say(nLinha,nCol,Transform(aDados[nX][4][nB][6][nC],"@E 99,999.99"),oFont9m)
				
			Next nC
			
			nCol += 200

			oRel:Say(nLinha,nCol,Transform(nTotInt,"@E 99,999.99"),oFont9mb)
		
		Next nB
		
		nLinha += 60
		
		oRel:Line(nLinha,0100,nLinha,3450)
		
		nLinha += 30
		
		// CNPJ Empresa
		oRel:Say(nLinha,0100,cEmpCNPJ,oFont9b)
		
		// Imprime Totais mensais
		
		nCol	:= 650
		nTotGer	:= 0
		
		For nD := 1 To 12
		
			nTotGer += nTotTit[nD]	// Total Geral
			
			nCol += 200
			
			oRel:Say(nLinha,nCol,Transform(nTotTit[nD],"@E 99,999.99"),oFont9mb)
		
		Next nD
			
		nCol += 200

		oRel:Say(nLinha,nCol,Transform(nTotGer,"@E 99,999.99"),oFont9mb)
		
		RodRelat()							
		oRel:EndPage()		
	Next nX
		
	oRel:Preview()	

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! CABRELAT	! Autor !Gilson Lima 		 ! Data ! 24/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Imprime Cabeçalho do Relatório								 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function CabRelat(cTitulo)

	Local cLogo :='\system\sindafep.bmp'
	
	oRel:StartPage()

	nLinha := 50

	If (File(cLogo))
		oRel:SayBitmap(nLinha+35,100,cLogo,310,200)
	EndIf
	
	nLinha += 100

	oRel:Say(nLinha,1600,cTitulo,oFont18)

	nLinha += 300

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RODRELAT	! Autor !Gilson Lima 		 ! Data ! 24/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Imprime Rodapé do Relatório								 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function RodRelat()

	// Imprimir Observações
	
	nLinRod := 1850
	oRel:Say(nLinRod,0100,cObserva1,oFont9b)
	nLinRod += 60
	oRel:Say(nLinRod,0100,cObserva2,oFont9b)
	
	// Imprimir Data
	aMeses := {'Janeiro','Fevereiro','Março','Abril',;
			   'Maio','Junho','Julho','Agosto',;
			   'Setembro','Outubro','Novembro','Dezembro'}
	
	nLinRod += 200
	oRel:Say(nLinRod,0100,'Curitiba, ' + cValToChar(Day(dDataBase)) + ' de ' + aMeses[Month(dDataBase)] + ' de ' + cValToChar(Year(dDataBase)),oFont10)
	
	// Imprimir Usuário
	cUsrNome := UsrFullName(RetCodUsr())
	//nLinRod += 60
	oRel:Say(nLinRod,1300,cUsrNome,oFont10)
	nLinRod += 50
	oRel:Say(nLinRod,1300,'Sindafep Saúde',oFont10)
	
	nLinRod += 60
	oRel:Line(nLinRod,0100,nLinRod,3450)
	nLinRod += 15
	oRel:Say(nLinRod,1200,'SINDAFEP - Sindicato dos Auditores Fiscais da Receita do Estado do Paraná',oFont9)
	nLinRod += 50
	oRel:Say(nLinRod,1250,'R. Alferes Ângelo Sampaio, 1793 - Batel - Curitiba/PR - CEP 80420-160',oFont9)
	nLinRod += 50
	oRel:Say(nLinRod,1300,'Tel.: (41)3221-5300 - Fax: (41)3222-2401 - www.sindafep.com.br',oFont9)

Return
