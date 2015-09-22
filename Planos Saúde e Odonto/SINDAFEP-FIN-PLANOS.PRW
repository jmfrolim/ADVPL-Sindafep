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
! Descricao ! Tela da Rotina Contas a Receber dos Planos de Saúde	 		 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function GFinPls()

	Private cPerg		:= "GFINPLS"
	Private aPergs		:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Competência","","","mv_ch1","C",7,0,0,"G","NAOVAZIO()","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 99/9999"})
		AADD(aPergs,{"Todos os Clientes?","","","mv_ch2","C",1,0,0,"C","","MV_PAR02","Sim","","","1","","Nao","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Grupo de Clientes","","","mv_ch3","C",6,0,0,"G","","MV_PAR03","","","","000001","","","","","","","","","","","","","","","","","","","","","ACY","","","",""})
		AADD(aPergs,{"Considerar Forma Cobr?","","","mv_ch4","C",1,0,0,"C","","MV_PAR04","Sim","","","1","","Nao","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Forma de Cobrança","","","mv_ch5","C",1,0,0,"C","","MV_PAR05","Financeiro","","","1","","Folha","","","","","Débito Automático","","","","","Boleto","","","","","","","","","","","","",""})
		AADD(aPergs,{"Data Vencto","","","mv_ch6","D",8,0,0,"G","NAOVAZIO()","MV_PAR06","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Aglutina Titulos?","","","mv_ch7","C",1,0,0,"C","","MV_PAR07","Sim","","","1","","Nao","","","","","","","","","","","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	If (!Pergunte(cPerg,.T.))
		Return
	EndIf

	//Chama função geração de títulos
	If MsgYesNo("Confirma a Geração dos Títulos?","Atenção")
		Processa({||U_GrTitPl()}) 
	Endif 
	
Return()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GRTITPL	! Autor !Gilson Lima 		 ! Data ! 10/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para geração do Contas a Receber dos Planos de Saúde	 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function GrTitPl()
	
	Local 	cNrTit	 	:= ''
	Local	cCliTipo	:= ''
	Local	cUltCFat	:= ''
	Local	nCont		:= 0
	Local	nContFam	:= 0
	Local	aCoPart 	:= {}
	Local	aPlano		:= {}
	Local	aAdic		:= {}
	
	Local 	lTitOk		:= .F.
	Local   aDados		:= {}
	
	Local	aIntegrant	:= {}
	Local	aIntegr		:= {}

	Local	nTotCoPart	:= 0
		
	Local	cPrefTit	:= ''
	Local	cTipoTit	:= ''
	Local	cNatTit		:= ''
	Local	nValTit		:= 0
	Local	cHistTit	:= ''
	
	Local 	lCob24h		:= .F.
	Local	dUDMes24h	:= ''
	Local	nDias24h	:= 0
	Local 	nVlCob24h	:= 0
	
	Local 	nVlINSS24	:= 0
	Local 	nVlINSS		:= 0
	Local	nVlCalc		:= 0
	
	Local	aTitulo		:= {}
	Local	aTitulos	:= {}

	Local	dPriDiaMes 	:= ''
	Local	dUltDiaMes	:= ''
	
	Local   cCliFCob	:= ''
	
	Private cCompet		:= MV_PAR01
	Private nTClientes	:= MV_PAR02
	Private cGrpCli		:= MV_PAR03
	Private nConsFCob	:= MV_PAR04
	Private nFormCob	:= MV_PAR05
	Private dDataVenc	:= MV_PAR06	
	Private lAglTit		:= IIF(MV_PAR07 == 1, .T., .F.)
	
	Private cPlDeclIRPF	:= GetNewPar('MV_PLIRPF',  'S')		// Plano de Saúde Declara IRPF ?	
	Private cCPDeclIRPF	:= GetNewPar('MV_COPIRPF',  'S')	// Co-Participação Declara IRPF ?
	Private cCPPercINSS	:= GetNewPar('MV_COPINSS',  4.5)	// Co-Participação Percentual INSS ?
	Private lTxAdmFun 	:= GetNewPar('MV_TXADMF', .F.)		// Taxa Adminstrativa para Funcionários
	
	Private lMsErroAuto := .F. // Determina se houve alguma inconsistencia na execucao da rotina
	
	// Define datas baseadas na Competência informada
	dPriDiaMes 	:= StoD(Substr(cCompet,4,4) + Substr(cCompet,1,2) + '01')
	dUltDiaMes 	:= LastDay(dPriDiaMes,0)
	cUltCFat	:= Substr(cCompet,4,4) + Substr(cCompet,1,2)
	
	// Busca Famílias
	dbSelectArea("SZH")
	SZH->(dbSetOrder(1))
	SZH->(dbGoTop())
	
	// Validação para trazer apenas as famílias ativas dentro da competência informada
	While SZH->(!EOF())
		
		If (AllTrim(DtoS(SZH->ZH_DATAINI)) == '' .Or. SZH->ZH_DATAINI <= dUltDiaMes);
			.And. (AllTrim(DtoS(SZH->ZH_DATATER)) == '' .Or. SZH->ZH_DATATER >= dPriDiaMes);
			.And. (AllTrim(SZH->ZH_ULTCFAT) == '' .Or. SZH->ZH_ULTCFAT < cUltCFat)
		
			// Busca tipo do cliente
			cCliTipo	:= POSICIONE("SA1",1,xFilial("SA1")+SZH->ZH_CODTIT,"A1_GRPVEN")
			cCliFCob	:= POSICIONE("SA1",1,xFilial("SA1")+SZH->ZH_CODTIT,"A1_ZFCOBPL")
			cCliLoja	:= POSICIONE("SA1",1,xFilial("SA1")+SZH->ZH_CODTIT,"A1_LOJA")
			
			If ((nTClientes != 1) .And. (cGrpCli != cCliTipo)) .OR. (nConsFCob == 1 .And. nFormCob != Val(cCliFCob)) // Se não for para faturar todas as famílias,
																							 						 // e o tipo do cliente não for o tipo escolhido, vai para a próxima família
																							 						 // ou o Tipo de cobrança for diferente da selecionada
				SZH->(dbSkip())
				Loop
			EndIf
			
			// Zera variáveis de valores
			nTotCoPart	:= 0
			nTotIntegr	:= 0
			
			// Zera Array de Integrantes
			aIntegrant	:= {}
			aIntegr		:= {}
			
			// Zera Array aDados
			aDados		:= {}
			
			// Zera informação de Titulo OK
			lTitOk		:= .F.
			
			// Zera Informações de INSS
			nVlINSS24	:= 0
			nVlINSS		:= 0
			nVlCalc		:= 0
				
			// Incrementa contador de Famílias
			nContFam++

			aTitulo		:= {} 	// Zera array do título
					
			// Busca integrantes da família
			dbSelectArea("SZJ")
			SZJ->(dbSetOrder(1))
			SZJ->(dbGoTop())
			
			SZJ->(dbSeek(xFilial('SZJ')+SZH->ZH_FAMILIA))
			
			// Busca integrantes ativos dentro da competência informada
			While SZJ->(!EOF()) .And. SZJ->ZJ_FAMILIA == SZH->ZH_FAMILIA
				
				If (AllTrim(DtoS(SZJ->ZJ_DATAINI)) == '' .Or. SZJ->ZJ_DATAINI <= dUltDiaMes);
					.And. (AllTrim(DtoS(SZJ->ZJ_DATATER)) == '' .Or. SZJ->ZJ_DATATER >= dPriDiaMes)
				
					// Zera array de integrante
					aIntegr := {}
					
					// Zera variáveis de Cob24H
					lCob24h		:= .F.
					dUDMes24h	:= ''
					nDias24h	:= 0
					nVlCob24h	:= 0
					
					// Verifica se tem que ser cobrado a inclusão 24h
					If ((SZJ->ZJ_INC24H == 'S') .And. (SZJ->ZJ_24HCob == 'N' .Or. SZJ->ZJ_24HCob == ' '))
						lCob24h 	:= .T.
						dUDMes24h 	:= LastDay(SZJ->ZJ_DATAINI,0)
						nDias24h	:= (dUDMes24h - SZJ->ZJ_DATAINI) + 1
					EndIf
					
					// Retorna array com plano
					If AllTrim(SZJ->ZJ_CODPLAN) != ''
						aPlano := u_CalcPlano(SZJ->ZJ_CODPLAN,SZJ->ZJ_DTNASC,cCompet)
					Else
						aPlano := Array(13)
					EndIf
					
					// Retorna Array com Adicionais
					If AllTrim(SZJ->ZJ_CODADIC) != ''
						aAdic	:= u_CalcAdic(SZJ->ZJ_CODADIC)
					Else
						aAdic	:= Array(10)
					EndIf

					/*****************************************
					* Calcula valor a ser cobrado do integrante
					*****************************************/
					
					// Se for Plano
					If AllTrim(SZJ->ZJ_CODPLAN) != ''
						// Valor do Plano
						nVlIntegr := aPlano[7]	// Mensalidade
						
						// Se Perc. Inss > 0, calcula
						If aPlano[10] > 0
							// Calcula o valor do INSS
							nVlINSS := nVlIntegr * (aPlano[10]/100)
							// Adiciona no valor total
							nVlIntegr += nVlINSS
						EndIf
								
						nVlIntegr += aPlano[8]	// Fundo
						
						// Taxa Administrativa
						If cCliTipo == '000003' .And. !lTxAdmFun	// 000003 = Titular for Funcionário e Parâmetro de Cobr. Tx. Adm = .F.
							nTxAdm := 0
						Else
							nTxAdm := aPlano[9]
						EndIf
						
						nVlIntegr += nTxAdm	

						// Se for funcionário, efetua Desconto
						If SZJ->ZJ_CODPAR == '000000' .And. cCliTipo == '000003'	// 000003 = Funcionário
							// Desconto no Plano
							nVlIntegr -= aPlano[11]
						EndIf
						
						// Se o total do plano for negativo, atribui zero
						If nVlIntegr < 0
							nVlIntegr := 0
						EndIf

						// Se cobrar 24h = .T.
						If lCob24h
							nVlCob24h := ((aPlano[7]+aPlano[8]+nTxAdm) / 30) * nDias24h
							
							// Se Perc. Inss > 0, calcula
							If aPlano[10] > 0							
								nVlCalc := (aPlano[7] / 30) * nDias24h 
								nVlINSS24 := nVlCalc * (aPlano[10]/100)
							Else
								nVlINSS24 := 0
							EndIf
							
							// Se for funcionário, efetua Desconto
							If SZJ->ZJ_CODPAR == '000000' .And. cCliTipo == '000003'	// 000003 = Funcionário
								nVlDFun24 := (aPlano[11] / 30) * nDias24h
							Else
								nVlDFun24 := 0								
							EndIf	
												
							nVl24h := nVlCob24h + nVlINSS24 - nVlDFun24
							
							// Se o valor da Cobr. 24h for negativa, Atribui Zero
							If nVl24h < 0
								nVl24h := 0
							EndIf
						Else
							nVl24h := 0	
						EndIf
						
						// Soma valor do Plano e da inclusao 24h
						nVlIntegr += nVl24h						

					EndIf
					
					// Se for Adicional
					If AllTrim(SZJ->ZJ_CODADIC) != ''
						
						// Vl Adicional
						nVlIntegr := aAdic[5]
						
						// Se Perc. Inss > 0, calcula
						If aAdic[8] > 0
							// Calcula o valor do INSS
							nVlINSS := nVlIntegr * (aAdic[8]/100)
							// Adiciona no valor total
							nVlIntegr += nVlINSS
						EndIf
												
						nVlIntegr += aAdic[6]	// Fundo

						// Taxa Administrativa
						If cCliTipo == '000003' .And. !lTxAdmFun	// 000003 = Titular for Funcionário e Parâmetro de Cobr. Tx. Adm = .F.
							nTxAdm := 0
						Else
							nTxAdm := aAdic[7]
						EndIf						
						
						nVlIntegr += nTxAdm	

						// Se for funcionário, efetua Desconto
						If SZJ->ZJ_CODPAR == '000000' .And. cCliTipo == '000003'	// 000003 = Funcionário
							// Desconto no Adicional
							nVlIntegr -= aAdic[9]
						EndIf

						// Se o total do adicional for negativo, atribui zero
						If nVlIntegr < 0
							nVlIntegr := 0
						EndIf						
						
						// Se cobrar 24h = .T.
						If lCob24h
							nVlCob24h := ((aAdic[5]+aAdic[6]+nTxAdm) / 30) * nDias24h
							
							// Se Perc. Inss > 0, calcula
							If aAdic[8] > 0							
								nVlCalc := (aAdic[5] / 30) * nDias24h 
								nVlINSS24 := nVlCalc * (aAdic[8]/100)
							Else
								nVlINSS24 := 0
							EndIf

							// Se for funcionário, efetua Desconto
							If SZJ->ZJ_CODPAR == '000000' .And. cCliTipo == '000003'	// 000003 = Funcionário
								nVlDFun24 := (aAdic[9] / 30) * nDias24h
							Else
								nVlDFun24 := 0								
							EndIf	
												
							nVl24h := nVlCob24h + nVlINSS24 - nVlDFun24
							
							// Se o valor da Cobr. 24h for negativa, Atribui Zero
							If nVl24h < 0
								nVl24h := 0
							EndIf
						Else
							nVl24h := 0	
						EndIf
						
						// Soma valor do Adicional e da inclusao 24h
						nVlIntegr += nVl24h		
			
					EndIf

					// Verifica se o valor total é negativo. Se for, ajusta para Zero
					If nVlIntegr < 0
						nVlIntegr := 0
					EndIf
					
					// Adiciona o valor do Depentende ao total dos Dependentes
					nTotIntegr += nVlIntegr
			
					/*********************************************/			

					// Acrescenta informações do dependente no array de integrantes
					
					aAdd(aIntegr,IIf(SZJ->ZJ_CODPAR == '000000', "T", "D"))			// [1] - Tipo de Integrante (T - Titular | D - Dependente)
					aAdd(aIntegr,IIf(SZJ->ZJ_CODPAR == '000000', cCliTipo, Space(6)))		// [2] - Origem do Integrante (grpven)
					aAdd(aIntegr,SZJ->ZJ_CODIGO)// [3] - Codigo do Integrante
					aAdd(aIntegr,SZJ->ZJ_LOJA)	// [4] - Loja do Integrante
					
					If SZJ->ZJ_CODPAR == '000000'
						aAdd(aIntegr,POSICIONE("SA1",1,xFilial("SA1")+SZJ->ZJ_CODIGO,"A1_NOME")) // [5] - Nome do Integrante
					Else
						aAdd(aIntegr,POSICIONE("SZG",1,xFilial("SZG")+SZJ->ZJ_CODIGO,"ZG_NOME")) // [5] - Nome do Integrante
					EndIf
					
					aAdd(aIntegr,u_ClIdade(IIF(StrZero(Month(SZJ->ZJ_DTNASC),2) == Substr(cCompet,1,2), CtoD('01/'+cCompet), SZJ->ZJ_DTNASC ), CtoD('01/'+cCompet))) // [6] - Idade
					
					// Planos
					aAdd(aIntegr,aPlano[3])			// [7] - Codigo do Grupo
					aAdd(aIntegr,aPlano[4])			// [8] - Descricao do Grupo
					aAdd(aIntegr,aPlano[5])			// [9] - Codigo do Plano
					aAdd(aIntegr,aPlano[6])			// [10] - Descricao do Plano
					aAdd(aIntegr,aPlano[1])			// [11] - Codigo da Faixa
					aAdd(aIntegr,aPlano[2])			// [12] - Descricao da Faixa
					aAdd(aIntegr,aPlano[12])		// [13] - Limite de Idade			
					aAdd(aIntegr,aPlano[13])		// [14] - Cobertura Plano
					aAdd(aIntegr,SZJ->ZJ_INC24H)	// [15] - Se é Inclusão 24H
					aAdd(aIntegr,IIf(lCob24h, 'S', 'N'))	// [16] - Se foi cobrada Inclusão 24H
					aAdd(aIntegr,nDias24h)			// [17] - Qtde de Dias cobrado da Inclusão 24H
					aAdd(aIntegr,nVlCob24h)			// [18] - Valor da Cobrança 24h
					
					// Adicionais
					aAdd(aIntegr,aAdic[1])			// [19] - Codigo do Grupo
					aAdd(aIntegr,aAdic[2])			// [20] - Descricao do Grupo
					aAdd(aIntegr,aAdic[3])			// [21] - Codigo do Adicional
					aAdd(aIntegr,aAdic[4])			// [22] - Descricao do Adicional
					
					// Planos
					If AllTrim(SZJ->ZJ_CODPLAN) != ''
						aAdd(aIntegr,cPlDeclIRPF)		// [23] - Decl. IRPF
					// Adicionais
					Else
						aAdd(aIntegr,aAdic[10])			// [23] - Decl. IRPF			
					EndIf
					
					// Valores Se for Planos
					If AllTrim(SZJ->ZJ_CODPLAN) != ''					
						aAdd(aIntegr,aPlano[7])		// [24] - Valor do Plano			
						aAdd(aIntegr,nVlIntegr) 	// [25] - Valor Cobrado
						aAdd(aIntegr,aPlano[8])		// [26] - Valor do Fundo			
						aAdd(aIntegr,nTxAdm)		// [27] - Valor da Taxa Administrativa Cobrada			
						aAdd(aIntegr,aPlano[10])	// [28] - Perc. INSS			
						aAdd(aIntegr,aPlano[11])	// [29] - Desconto Funcionario			
					EndIf
					
					// Valores Se for Adicionais
					If AllTrim(SZJ->ZJ_CODADIC) != ''
						aAdd(aIntegr,aAdic[5])		// [24] - Valor do Adicional			
						aAdd(aIntegr,nVlIntegr)  	// [25] - Valor Cobrado
						aAdd(aIntegr,aAdic[6])		// [26] - Valor do Fundo			
						aAdd(aIntegr,nTxAdm)		// [27] - Valor da Taxa Administrativa Cobrada			
						aAdd(aIntegr,aAdic[8])		// [28] - Perc. INSS			
						aAdd(aIntegr,aAdic[9])		// [29] - Desconto Funcionario			
					EndIf

					// Adiciona array com dados do dependente ao array principal de Integrantes
					aAdd(aIntegrant,aIntegr)			

				EndIf
								   
				SZJ->(dbSkip())			    
			End
			
			SZJ->(dbCloseArea())
			
			// Busca informações da Coparticipação da família
			aCoPart := u_CalcCoPar(Substr(SZH->ZH_FAMILIA,1,7),cCompet)
			
			// Totaliza Coparticipação da família
			For Nx := 1 To Len(aCoPart)
				If aCopart[nX][10] == 'C'
					nTotCoPart += aCoPart[nX][9]
				Else
					nTotCoPart -= aCopart[nX][9]
				EndIf
			Next Nx	
			
			// Calcula INSS Co-Participação
			// Baseado no percentual de Parâmetro
			nTotCoPart += nTotCoPart * (cCPPercINSS/100)
			
			// Define o nr. do título baseado na competência e tipo de cliente
			cNrTit := u_DefNrTit(cCompet,cValtoChar(Val(cCliTipo)))
			
			// Define o Prefixo e Natureza e Histórico do Título
			If SZH->ZH_CODGRP == '000001' 		// 000001 = Unimed
				
				cPrefTit := 'PLS'				// [PREFIXO] PLS = Planos de Saúde
				cNatTit	 := '1102001006'		// [NATUREZA] = Mensalidade Saúde
				cHistTit := 'MENS SAUDE ' + Substr(cCompet,1,2) + "/" + Substr(cCompet,6,2)
			
			ElseIf SZH->ZH_CODGRP == '000002'	// 000002 = Odontouni
				
				cPrefTit := 'PLO'				// [PREFIXO] PLO = Planos Odontológicos
				cNatTit	 := '1102001007'		// [NATUREZA] = Mensalidade Odonto
				cHistTit := 'MENS ODONTO ' + Substr(cCompet,1,2) + "/" + Substr(cCompet,6,2)
			
			EndIf
			
			// Define o Tipo do Título
			Do Case
				Case cCliFCob == '1'
					cTipoTit	:= 'RC'
				Case cCliFCob == '2'
					cTipoTit	:= 'FOL'
				Case cCliFCob == '3'
					cTipoTit	:= 'DEB'
				Case cCliFCob == '4'
					cTipoTit	:= 'BOL'
				OtherWise
					cTipoTit	:= 'FT'
			EndCase
					
			// Trata datas de vencimento
			dDtVenc		:= IIF (AllTrim(DtoS(dDataVenc)) != '', dDataVenc, dDataBase)	
			dDtVencR	:= LastDay(dDtVenc,3)
			
			// Calcula o valor total do titulo
			nValTit 	:= nTotIntegr + nTotCoPart
			
			// Monta array para ExecAuto
			aTitulo := {{"E1_PREFIXO",	cPrefTit,		Nil},;	// Prefixo do Título
						{"E1_NUM",		cNrTit,			Nil},;	// Número do Título
						{"E1_TIPO",		cTipoTit,		Nil},;	// Tipo do Título
						{"E1_NATUREZ",	cNatTit,		Nil},;	// Natureza do Título
						{"E1_CLIENTE",	SZH->ZH_CODTIT,	Nil},;	// Código do Titular
						{"E1_LOJA",		cCliLoja,		Nil},;	// Loja do Titular
						{"E1_EMISSAO",	dDataBase,		Nil},;	// Data de Emissão
						{"E1_VENCTO",	dDtVenc,		Nil},;	// Data de Vencimento
						{"E1_VENCREA",	dDtVencR,		Nil},;	// Data de Vencimento Real
						{"E1_VALOR",	nValTit,		Nil},;	// Valor do Título
						{"E1_MSFIL",	cFilAnt,		Nil},;	// Filial Corrente
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
				
				// Informa que o Titulo foi criado com sucesso
				lTitOk	:= .T.
				
			EndIf
	
			// Atribui competência de faturamento no cadastro de famílias
			RecLock("SZH",.F.)
	
				SZH->ZH_ULTCFAT := cUltCFat			
				
			SZH->(MsUnlock())
			
			// Grava informações do faturamento na tabela de histórico se geração do título está OK
			If lTitOk
						
				If cPrefTit != 'PLO'
			
					// Grava informações de cobrança de Inclusão 24H
					For Nz := 1 To Len(aIntegrant)
						If (aIntegrant[nZ][15] == 'S')

							dbSelectArea("SZJ")
							
							SZJ->(dbSetOrder(2))
							SZJ->(dbGoTop())
							SZJ->(dbSeek(xFilial("SZJ")+aIntegrant[nZ][3]))
							
							RecLock('SZJ',.F.)
								SZJ->ZJ_24HCOB := 'S'
							SZJ->(MsUnlock())

						EndIf
					Next Nz
	
					// Marca Flag como Processada na tabela de CoParticipação
					// baseado no Array retornado pela rotina de CoParticipação
					For Nx := 1 To Len(aCoPart)
						
						dbSelectArea("SZ1")
						SZ1->(dbGoTo(aCoPart[Nx][1]))
						
						RecLock("SZ1",.F.)
						
							SZ1->Z1_PROCESS := 'S'
						
						SZ1->(MsUnlock())
						
						SZ1->(dbCloseArea())
					Next Nx
				EndIf
			
				aAdd(aDados,SZH->ZH_FAMILIA)
				aAdd(aDados,cUltCFat)
				aAdd(aDados,POSICIONE("SA1",1,xFilial("SA1")+SZH->ZH_CODTIT,"A1_FILIAL"))
					
				GravHist(aTitulo, aIntegrant, aCoPart, aDados)
			EndIf
		EndIf
			
		SZH->(dbSkip())
	End 
	
	SZH->(dbCloseArea())
	
	// Calcula CoParticipação das famílias não ativas mas que tenham CoParticipação a ser cobrada
	nTitCoPart := u_ClCoParP(cCompet)
	
	// Se selecionado para Aglutinar Títulos
	If lAglTit
		
		// Aglutina titulos gerados
		u_AgTitPl1(2, dDataVenc, dDataVenc, nConsFCob, nFormCob)
		
	EndIf
	
	// Exibe mensagem de conclusão de geração dos títulos
	If nCont > 0
		MSGInfo(cValToChar(nContFam) + 'família(s) processada(s) e ' + cValToChar(nCont) + ' título(s) gerado(s) com sucesso.')
		If nTitCoPart > 0
			MSGInfo(cValToChar(nTitCoPart) + ' título(s) de CoParticição Unimed gerado(s) com sucesso.')
		EndIf
	ElseIf (nTitCoPart > 0) .And. (nCont <= 0) 
		MSGInfo(cValToChar(nTitCoPart) + ' título(s) de CoParticição Unimed gerado(s) com sucesso.')
	Else
		MSGInfo("Nenhum título a ser gerado com os parâmetros atuais.")
	EndIf
	
Return ()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! DefNrTit	! Autor !Gilson Lima 		 ! Data ! 12/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao para geração do número do Título						 !
+----------------------------------------------------------------------------*/
User Function DefNrTit(cCompet,cTipo)

	Local aArea		 := GetArea()
	Local cAnoCompet := Substr(cCompet,6,2)
	Local cMesCompet := Substr(cCompet,1,2)
	Local cNumero	 := ''
	Local cQuery	 := '' 
	
	cQuery	:= "SELECT TOP 1 SE1.E1_NUM FROM " + RetSqlName('SE1') + " SE1"
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
	
	SE1->(dbCloseArea())
	
	If AllTrim(cNumero) == ''
		cTitulo := cAnoCompet+cMesCompet+cTipo+"0001"
	Else
		cTitulo := soma1(cNumero)
	EndIf
	
	RestArea(aArea)
	
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
	Local nFundo	:= 0
	Local nTxAdm	:= 0
	Local nPerInss	:= 0
	Local nDesFunc	:= 0	
	
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
	
	cQuery		:= "SELECT SZF.ZF_CODIGO, SZF.ZF_VALOR, SZF.ZF_FUNDO, SZF.ZF_TXADM, SZF.ZF_PERINSS, SZF.ZF_DESFUNC "
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
		nFundo		:= SZF->ZF_FUNDO
		nTxAdm		:= SZF->ZF_TXADM
		nPerInss	:= SZF->ZF_PERINSS
		nDesFunc	:= SZF->ZF_DESFUNC
		
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
	aAdd(aPlano,nFundo)		// [8] Fundo
	aAdd(aPlano,nTxAdm)		// [9] Taxa Administrativa
	aAdd(aPlano,nPerInss)	// [10] Percentual INSS
	aAdd(aPlano,nDesFunc)	// [11] Desconto Funcionario
	aAdd(aPlano,nLimIdade)	// [12] Limite de Idade
	aAdd(aPlano,nCobPlan)	// [13] Cobertura do Plano
	
	RestArea(aArea)

Return(aPlano)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! ClFaixaPl	! Autor !Gilson Lima 		 ! Data ! 26/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao cálculo da idade baseada na Dt. de Nascimento e Plano	 !
+----------------------------------------------------------------------------*/
User Function CalcAdic(cCodAdic)
	
	Local aArea := GetArea()
	Local aAdic	:= {}
	
	cQuery		:= "SELECT SZN.ZN_CODIGO, SZN.ZN_CODGRP, SZN.ZN_DESCR, SZN.ZN_VALOR, SZN.ZN_FUNDO, SZN.ZN_TXADM, SZN.ZN_PERINSS, SZN.ZN_DESFUNC, SZN.ZN_IRPF "
	cQuery		+= " FROM " + RetSqlName("SZN") + " SZN "
	cQuery		+= "WHERE SZN.ZN_CODIGO = '" + cCodAdic + "' "
	cQuery		+= "AND SZN.D_E_L_E_T_ = ' '"

	If Select("SZN") <> 0
		SZN->(dbCloseArea())
	EndIf
	
	DbUseArea(.T., 'TOPCONN', TCGenQry(,,cQuery), 'SZN', .F.,.T.)
	
	DbSelectArea("SZN")
	SZN->(DbGoTop())
	
	While SZN->(!EOF())
		cCodAdic	:= SZN->ZN_CODIGO
		cCodGrp		:= SZN->ZN_CODGRP
		cDescr		:= SZN->ZN_DESCR
		nValor		:= SZN->ZN_VALOR
		nFundo		:= SZN->ZN_FUNDO
		nTxAdm		:= SZN->ZN_TXADM
		nDesFunc	:= SZN->ZN_DESFUNC
		nPerInss	:= SZN->ZN_PERINSS
		cIRPF		:= SZN->ZN_IRPF
		
		Exit
	End

	SZN->(dbCloseArea())
	
	cGrpDesc	:= Posicione("SZD",1,xFilial("SZD")+cCodGrp,"ZD_DESCR")
	
	aAdd(aAdic,cCodGrp)	// [1] Código do Grupo
	aAdd(aAdic,cGrpDesc)// [2] Descricao do Grupo
	aAdd(aAdic,cCodAdic)// [3] Código Adicional
	aAdd(aAdic,cDescr)	// [4] Descricao do Adicional
	aAdd(aAdic,nValor)	// [5] Valor
	aAdd(aAdic,nFundo)	// [6] Fundo
	aAdd(aAdic,nTxAdm)	// [7] Taxa Administrativa
	aAdd(aAdic,nPerInss)// [8] Percentual INSS
	aAdd(aAdic,nDesFunc)// [9] Desconto Funcionario
	aAdd(aAdic,cIRPF)	// [10] Decl. IRFP?
	
	RestArea(aArea)

Return(aAdic)

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
		If (SZ1->Z1_COMPFAT < cCompFat) .And. (SZ1->Z1_PROCESS == 'N')
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
				SZ1->Z1_DEVCOB,;	// [10]- Devolução ou Cobrança
				cCPPercINSS;		// [11]- Percentual de INSS
				})
		EndIf
	
		SZ1->(dbSkip())
	End
	
	SZ1->(dbCloseArea())
	
	RestArea(aArea)
Return(aCoPart)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GravHist	! Autor !Gilson Lima 		 ! Data ! 17/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao gravação de histórico dos ítens que compôem os títulos	 !
!           ! gerados. 														 !
+----------------------------------------------------------------------------*/
Static Function GravHist(aTitulo, aIntegrant, aCoPart, aDados)
	
	Local aArea := GetArea()
	
	If Len(aTitulo) > 0

		// Grava Informações do Título
		dbSelectArea("SZL")
		RecLock('SZL', .T.)
			
			SZL->ZL_FILIAL	:= xFilial('SZL')
			SZL->ZL_TPDADO	:= 'F'				// Financeiro
			SZL->ZL_COMPET	:= aDados[2]		// Competência
			SZL->ZL_FAMILIA	:= aDados[1]		// Familia
			SZL->ZL_TITPREF	:= aTitulo[1][2]
			SZL->ZL_TITNUM	:= aTitulo[2][2]
			SZL->ZL_TITFIL	:= aDados[3]		// Filial do Título
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
	
	If Len(aIntegrant) > 0
	
		// Grava Informações dos Integrantes x Planos
		For Nz := 1 To Len(aIntegrant)
	
			dbSelectArea("SZL")
			RecLock('SZL', .T.)
	
				SZL->ZL_FILIAL	:= xFilial('SZL')
				SZL->ZL_TPDADO	:= 'P'				// Planos
				SZL->ZL_COMPET	:= aDados[2]		// Competência
				SZL->ZL_FAMILIA	:= aDados[1]		// Família
				SZL->ZL_TITPREF	:= aTitulo[1][2]
				SZL->ZL_TITNUM	:= aTitulo[2][2]
				SZL->ZL_TITLOJA	:= aTitulo[6][2]
				SZL->ZL_TITFIL	:= aDados[3]		// Filial do Título						
	
				SZL->ZL_PLINTTP	:= aIntegrant[Nz][1]
				SZL->ZL_PLINTOR	:= aIntegrant[Nz][2]
				SZL->ZL_PLINTCD	:= aIntegrant[Nz][3]
				SZL->ZL_PLINTLJ	:= aIntegrant[Nz][4]
				SZL->ZL_PLINTNM	:= aIntegrant[Nz][5]
				SZL->ZL_PLINTID	:= aIntegrant[Nz][6]
				SZL->ZL_PLGRPCD	:= aIntegrant[Nz][7]
				SZL->ZL_PLGRPDS	:= aIntegrant[Nz][8]
				SZL->ZL_PLCOD	:= aIntegrant[Nz][9]
				SZL->ZL_PLDESCR	:= aIntegrant[Nz][10]
				SZL->ZL_PLFXCOD	:= aIntegrant[Nz][11]
				SZL->ZL_PLFXDSC	:= aIntegrant[Nz][12]
				SZL->ZL_PLIDLIM	:= aIntegrant[Nz][13]
				SZL->ZL_PLCOB	:= aIntegrant[Nz][14]
				SZL->ZL_PLINC24	:= aIntegrant[Nz][15]
				SZL->ZL_PLCOB24	:= aIntegrant[Nz][16]
				SZL->ZL_PLDIA24	:= aIntegrant[Nz][17]
				SZL->ZL_PLVL24	:= aIntegrant[Nz][18]
				SZL->ZL_ADGRPCD := aIntegrant[Nz][19]
				SZL->ZL_ADGRPDS := aIntegrant[Nz][20]
				SZL->ZL_ADCOD 	:= aIntegrant[Nz][21]
				SZL->ZL_ADDESCR	:= aIntegrant[Nz][22]
				SZL->ZL_ADDIRPF	:= aIntegrant[Nz][23]
				SZL->ZL_PLVL	:= aIntegrant[Nz][24]
				SZL->ZL_PLVLCOB	:= aIntegrant[Nz][25]
				SZL->ZL_PLVLFUN	:= aIntegrant[Nz][26]
				SZL->ZL_PLVLTXA	:= aIntegrant[Nz][27]
				SZL->ZL_PLPINSS	:= aIntegrant[Nz][28]
				SZL->ZL_PLPFUNC	:= aIntegrant[Nz][29]
				
			SZL->(MsUnlock())
			SZL->(dbCloseArea())
		Next
	EndIf

	If Len(aCoPart) > 0
		// Grava Informações de CoParticipação
		For Ny := 1 To Len(aCoPart)
	
			dbSelectArea("SZL")
			RecLock('SZL', .T.)

				SZL->ZL_FILIAL	:= xFilial('SZL')
				SZL->ZL_TPDADO	:= 'C'				// CoParticipação
				SZL->ZL_COMPET	:= aDados[2]		// Competência
				SZL->ZL_FAMILIA	:= aDados[1]		// Família
				SZL->ZL_TITPREF	:= aTitulo[1][2]
				SZL->ZL_TITNUM	:= aTitulo[2][2]
				SZL->ZL_TITLOJA	:= aTitulo[6][2]
				SZL->ZL_TITFIL	:= aDados[3]		// Filial do Título						
				
				SZL->ZL_ADDIRPF	:= cCPDeclIRPF		// Se declara ou não IRPF
				
				SZL->ZL_CPRECNO := aCoPart[Ny][1]
				SZL->ZL_CPBENCD := aCoPart[Ny][3]
				SZL->ZL_CPBENNM := aCoPart[Ny][4]
				SZL->ZL_CPATDDT := aCoPart[Ny][5]
				SZL->ZL_CPATDTP := aCoPart[Ny][6]
				SZL->ZL_CPATDDS := aCoPart[Ny][7]
				SZL->ZL_CPPRNOM := aCoPart[Ny][8]
				SZL->ZL_CPVLSER := aCoPart[Ny][9]
				SZL->ZL_CPCOBDV	:= aCoPart[Ny][10]
				SZL->ZL_CPPINSS	:= aCoPart[nY][11]	
	
			SZL->(MsUnlock())
			SZL->(dbCloseArea())
		Next Ny						
	EndIf
	
	RestArea(aArea)
Return()

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! ClCoParP	! Autor !Gilson Lima 		 ! Data ! 17/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao calcula co-participação								 !
!           ! 		 														 !
+----------------------------------------------------------------------------*/
User Function ClCoParP(cCompet)

	Local aArea 		:= GetArea()
	Local cCompFat		:= Substr(cCompet,4,4) + Substr(cCompet,1,2)
	Local cQuery		:= ''
	Local cCodTit		:= ''
	Local aFamilias		:= {}
	Local cCliTipo		:= ''
	Local cNrTit 		:= ''
	Local cPrefTit 		:= ''
	Local cNatTit	 	:= ''
	Local cTipoTit 		:= ''
	Local dDtVenc		:= ''	
	Local dDtVencR		:= ''
	Local cHistTit 		:= ''
	Local nValTit 		:= 0
	Local aTitulo		:= {}
	Local aCoPart		:= {}
	Local nTotCoPart 	:= 0
	Local cNrTitulo		:= ''
	Local lTitOk		:= .F.
	Local aDados		:= {}
	Local aIntegrant	:= {}
	Local nTitCoPart	:= 0
	Local cCliFCob		:= ''
	
	cQuery		:= "SELECT Distinct(SZ1.Z1_FAMILIA)"
	cQuery		+= " FROM " + RetSqlName("SZ1") + " SZ1 "
	cQuery		+= "WHERE SZ1.Z1_COMPFAT < '" + cCompFat + "' "
	cQuery		+= "AND SZ1.Z1_PROCESS = 'N' "
	cQuery		+= "AND SZ1.D_E_L_E_T_ = ' '"

	If Select("SZ1") <> 0
		SZ1->(dbCloseArea())
	EndIf
	
	DbUseArea(.T., 'TOPCONN', TCGenQry(,,cQuery), 'SZ1', .F.,.T.)
	
	DbSelectArea("SZ1")
	SZ1->(DbGoTop())
	
	While SZ1->(!EOF())
		
		aAdd(aFamilias,SZ1->Z1_FAMILIA)
	
		SZ1->(dbSkip())
	End
	
	SZ1->(dbCloseArea())

	For nY := 1 to Len(aFamilias)
			
		cCodTit 	:= POSICIONE("SZH",1,xFilial("SZH") + PadR(aFamilias[nY], 15, ' '),"ZH_CODTIT")
		cLojaTit 	:= POSICIONE("SZH",1,xFilial("SZH") + PadR(aFamilias[nY], 15, ' '),"ZH_LOJA")
		cCliFCob	:= POSICIONE("SA1",1,xFilial("SA1")+cCodTit,"A1_ZFCOBPL")
		
		If (allTrim(cCodTit) != '') .And. (nConsFCob == 1 .And. nFormCob != Val(cCliFCob))

			aCoPart		:= {}
			nTotCoPart 	:= 0
			lTitOk		:= .F.
			cCliTipo	:= POSICIONE("SA1",1,xFilial("SA1")+cCodTit,"A1_GRPVEN")
						
			aCoPart 	:= u_CalcCoPar(Substr(aFamilias[nY],1,7),cCompet)

			// Totaliza Coparticipação da família
			For Nx := 1 To Len(aCoPart)
				If aCopart[nX][10] == 'C'
					nTotCoPart += aCoPart[nX][9]
				Else
					nTotCoPart -= aCopart[nX][9]
				EndIf
			Next Nx

			// Calcula INSS Co-Participação
			// Baseado no percentual de Parâmetro
			nTotCoPart += nTotCoPart * (cCPPercINSS/100)
			
			// Define o nr. do título baseado na competência e tipo de cliente
			cNrTit 		:= u_DefNrTit(cCompet,cValtoChar(Val(cCliTipo)))
			
			// Define o Prefixo e Natureza do Título
			cPrefTit 	:= 'PLS'				// [PREFIXO] PLS = Planos de Saúde
			cNatTit	 	:= '1102001006'			// [NATUREZA] = Mensalidade Saúde
			
			// Define o Tipo do Título
			Do Case
				Case cCliFCob == '1'
					cTipoTit	:= 'RC'
				Case cCliFCob == '2'
					cTipoTit	:= 'FOL'
				Case cCliFCob == '3'
					cTipoTit	:= 'DEB'
				Case cCliFCob == '4'
					cTipoTit	:= 'BOL'
				OtherWise
					cTipoTit	:= 'FT'
			EndCase
			
			// Trata datas de vencimento
			dDtVenc		:= IIF (AllTrim(DtoS(dDataVenc)) != '', dDataVenc, dDataBase)	
			dDtVencR	:= LastDay(dDtVenc,3)
			
			// Define o Histórico do Titulo
			cHistTit 	:= 'CoPart. Unimed - ' + cCompet
			
			// Calcula o valor total do titulo
			nValTit 	:= nTotCoPart
			
			// Monta array para ExecAuto
			aTitulo := {{"E1_PREFIXO",	cPrefTit,		Nil},;	// Prefixo do Título
						{"E1_NUM",		cNrTit,			Nil},;	// Número do Título
						{"E1_TIPO",		cTipoTit,		Nil},;	// Tipo do Título
						{"E1_NATUREZ",	cNatTit,		Nil},;	// Natureza do Título
						{"E1_CLIENTE",	cCodTit,		Nil},;	// Código do Titular
						{"E1_LOJA",		cLojaTit,		Nil},;	// Loja do Titular
						{"E1_EMISSAO",	dDataBase,		Nil},;	// Data de Emissão
						{"E1_VENCTO",	dDtVenc,		Nil},;	// Data de Vencimento
						{"E1_VENCREA",	dDtVencR,		Nil},;	// Data de Vencimento Real
						{"E1_VALOR",	nValTit,		Nil},;	// Valor do Título
						{"E1_MSFIL",	cFilAnt,		Nil},;	// Filial Corrente
						{"E1_HIST",		cHistTit,		Nil}}	// Histórico do Título
	
			// Executa Geração do Título se o valor for maior que 0
			If nValTit > 0
				lMsErroAuto	:= .F.
				
				MSExecAuto({|x,y| Fina040(x,y)},aTitulo,3) //3- Inclusão, 4- Alteração, 5- Exclusão
				
				If lMsErroAuto
					MostraErro()
					Return()
				EndIf
				
				// Informa que o Titulo foi criado com sucesso
				lTitOk	:= .T.
				
				// Incrementa contador de títulos
				nTitCoPart++
				
			EndIf

			// Grava informações do faturamento na tabela de histórico se geração do título está OK
			If lTitOk
			
				// Marca Flag como Processada na tabela de CoParticipação
				// baseado no Array retornado pela rotina de CoParticipação
				For nX := 1 To Len(aCoPart)
					
					dbSelectArea("SZ1")
					SZ1->(dbGoTo(aCoPart[Nx][1]))
					
					RecLock("SZ1",.F.)
						SZ1->Z1_PROCESS := 'S'
					SZ1->(MsUnlock())
					SZ1->(dbCloseArea())
				Next nX			
			
				aAdd(aDados,aFamilias[nY])
				aAdd(aDados,cCompFat)
				aAdd(aDados,POSICIONE("SA1",1,xFilial("SA1")+cCodTit,"A1_FILIAL"))
					
				GravHist(aTitulo, aIntegrant, aCoPart, aDados)

			EndIf
		EndIf
	Next nY
	
	RestArea(aArea)
	
Return(nTitCoPart)
