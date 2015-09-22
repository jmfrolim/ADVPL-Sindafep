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
!Nome 			   ! IMP5MH01 												 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Importação do arquivo 5MH de mensalidades descontadas	 !
!				   ! em Folha de Pagamento e enviado pela Celepar	 		 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 08/12/14 												 !
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
! Programa 	! IMP5MH01	! Autor !Gilson Lima 		 ! Data ! 08/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada no menu para a importação do arquivo			 !
! 			! 5MH - Mensalidades											 !
+----------------------------------------------------------------------------*/
User Function IMP5MH01()
	
	Private cFile	:= ""
	Private cPerg	:= "IMP5MH01"
	Private aPergs	:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Importar 5MH?","","","mv_ch1","C",1,0,0,"C","","MV_PAR01","Sim","","","","","Não","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Arquivo Importação","Arquivo Importação","Arquivo Importação","mv_ch2","C",60,0,0,"G","","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
		AADD(aPergs,{"Processar Baixas?","","","mv_ch3","C",1,0,0,"C","","MV_PAR03","Sim","","","","","Não","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Competência","","","mv_ch4","C",7,0,0,"G","","MV_PAR04","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 99/9999"})
		AADD(aPergs,{"Atualiza Vlr. Mensalidades","","","mv_ch5","C",1,0,0,"C","","MV_PAR05","Sim","","","","","Não","","","","","","","","","","","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)
	
	// Chama a função de importação
	If MsgYesNo("Confirma Processamento 5MH?","Atenção")
		Processa({||IMP5MH02()})
	EndIf
Return


/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! IMP5MH02	! Autor !Gilson Lima 		 ! Data ! 26/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Processa a baixa do arquivo		 							 !
! 			! 5MH															 !
+----------------------------------------------------------------------------*/
Static Function IMP5MH02()

	Local cCompet 	:= ''
	Local cQuery	:= ''
	Local a5MH:= {}

	// Se Importar arquivo = S, processa importação
	If MV_PAR01 == 1
		IMP5MH03()
	EndIf
	
	// Se Processar Baixas = S
	If MV_PAR03 == 1
	
		IncProc("Processando baixas ...")
		
		a5MH := {}
		cCompet	:= Substr(MV_PAR04,4,4) + Substr(MV_PAR04,1,2)
		
		dbSelectArea("SZY")
		SZY->(dbSetOrder(2))
		SZY->(dbGoTop())
		SZY->(dbSeek(xFilial("SZY")+cCompet))
		
		While SZY->(!EOF() .And. SZY->ZY_ANOMES == cCompet)
			
			// Executa apenas para os não processados
			If SZY->ZY_PROCESS == 'N'
			
				// Adicionar em um array os dados não processados, analizando se já existe o cpf. Se sim, soma valor, se não, adiciona novo registro.
				// a5MH[1] = CPF
				// a5MH[2] = Valor
				lIncl := .F.				
				If Len(a5MH) > 0
					
					For nE := 1 To Len(a5MH)
						If a5MH[nE][1] == SZY->ZY_CPF
							a5MH[nE][2] := a5MH[nE][2] + SZY->ZY_VALOR 
							lIncl := .T.
							Exit
						EndIf
					Next nE
					
					If !lIncl
						aAdd(a5MH,{SZY->ZY_CPF, SZY->ZY_VALOR})
					EndIf
				Else
					aAdd(a5MH,{SZY->ZY_CPF, SZY->ZY_VALOR})
				EndIf

			EndIf
		
			SZY->(dbSkip())	
		End
		
		SZY->(dbCloseArea()) 
		
		// Processa Baixas e Atualização de valores de mensalidades
		For nD := 1 To Len(a5MH)		
					
			// Se o retorno da baixa do título for positivo		
			If Baixa5MH(cCompet,a5MH[nD][1],a5MH[nD][2])
				
				dbSelectArea("SZY")
				SZY->(dbSetOrder(1))
				SZY->(dbGoTop())
				SZY->(dbSeek(xFilial("SZY")+cCompet+a5MH[nD][1]))
				
				While SZY->(!EOF()) .And. SZY->(ZY_ANOMES) == cCompet .And. SZY->(ZY_CPF) == a5MH[nD][1]
				
					// Grava informação de processado na tabela SZY				
					RecLock("SZY", .F.)
						SZY->ZY_PROCESS := 'S'
					SZY->(MsUnLock())
					
					SZY->(dbSkip())
				End
				
				SZY->(dbCloseArea())
			EndIf

			// Atualiza Valor da Mensalidade
			If MV_PAR05 == 1
				
				cCpf	:= PadR(Substr(a5MH[nD][1],3), 14, ' ')
				
				// Busca Código do Cliente baseado no CPF
				cCodCli := POSICIONE("SA1",3,XFILIAL('SA1')+cCpf,"A1_COD")
				
				dbSelectArea('SA1')
				SA1->(dbSetOrder(3))
				SA1->(dbSeek(XFILIAL('SA1')+cCpf))
				
				// Atualiza o valor das mensalidades
				While SA1->(!EOF()) .And. SA1->A1_CGC == cCpf
					RecLock("SA1", .F.)
						SA1->A1_VLRMENS := a5MH[nD][2]
						// Atualiza forma de cobrança (temporário)
						// SA1->A1_FORMCOB := '2'
					SA1->(MsUnLock())
					SA1->(dbSkip())
				End
				
				SA1->(dbCloseArea())					
			EndIf
		Next nD
		
	EndIf
	
	MsgInfo("Processamento 5MH concluído!","Concluído")
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! Baixa5MH	! Autor !Gilson Lima 		 ! Data ! 26/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Localiza título com as informações passadas por parâmetros	 !
! 			! e efetua a baixa do mesmo, retornando se operação foi positiva !
+----------------------------------------------------------------------------*/
Static Function Baixa5MH(cCompet,cCPF,nValor)
	
	Local aArea			:= GetArea()
	Local cQuery		:= ''
	Local cMesAno		:= Substr(cCompet,5,2) + '/' + Substr(cCompet,1,4)
	Local cHistTit 		:= PadR('MENS SINDICAL ' + Substr(cMesAno,1,2) + "/" + Substr(cMesAno,6,2) ,25 ,' ')
	Local cCodCli		:= ''
	Local aTitulo		:= {}
	Local cHistBx		:= 'Processamento 5MH'
	Local lMsErroAuto 	:= .F.
	Local lOk			:= .F.
	Local nVlDifer		:= 0
	
	// Ajusta CPF do arquivo 5MH
	cCPF 				:= PadR(Substr(cCPF,3), 14, ' ')

	// Busca Código do Cliente baseado no CPF
	cCodCli				:= POSICIONE("SA1",3,XFILIAL('SA1')+cCPF,"A1_COD")
	
	dbSelectArea('SE1')
	SE1->(dbSetOrder(2))
	SE1->(dbGoTop())
	SE1->(dbSeek(XFILIAL("SE1")+cCodCli))

	While SE1->(!EOF()) .And. SE1->E1_CLIENTE == cCodCli
		If (SE1->E1_STATUS == 'A' .And. SE1->E1_PREFIXO == 'MSL' .And. cHistTit $ SE1->E1_HIST)
		
			// Ajusta os valores do título conforme o recebido via 5MH
			If SE1->E1_VALOR != nValor
				RecLock("SE1", .F.)
					// Valor 5MH Maior que Valor do Título
					If nValor > SE1->E1_VALOR
						nVlDifer := nValor - SE1->E1_VALOR
						
						SE1->E1_ACRESC  := nVlDifer
						SE1->E1_SDACRES := nVlDifer
						
					// Valor 5MH Menor que Valor do Título
					ElseIf nValor < SE1->E1_VALOR
						nVlDifer := SE1->E1_VALOR - nValor
						
						SE1->E1_DECRESC  := nVlDifer
						SE1->E1_SDDECRE  := nVlDifer
					EndIf 
				SE1->(MsUnLock())
			EndIf
			
			// Adiciona os dados para baixa do título
			aAdd(aTitulo,{"E1_PREFIXO",   SE1->E1_PREFIXO, 	Nil})
			aAdd(aTitulo,{"E1_NUM", 	  SE1->E1_NUM, 	 	Nil})
			aAdd(aTitulo,{"E1_PARCELA",   SE1->E1_PARCELA,	Nil})
			aAdd(aTitulo,{"E1_TIPO", 	  SE1->E1_TIPO, 	Nil})
			aAdd(aTitulo,{"E1_CLIENTE",   SE1->E1_CLIENTE, 	Nil})
			aAdd(aTitulo,{"E1_LOJA", 	  SE1->E1_LOJA, 	Nil})
			aAdd(aTitulo,{"AUTJUROS", 	  0, 	 			Nil})
			aAdd(aTitulo,{"AUTMULTA", 	  0, 	 			Nil})
			aAdd(aTitulo,{"AUTVALREC", 	  nValor,			Nil})
			aAdd(aTitulo,{"AUTMOTBX", 	  "5MH", 	 		Nil})
			aAdd(aTitulo,{"AUTDTBAIXA",   dDataBase, 	 	Nil})
			aAdd(aTitulo,{"AUTDTCREDITO", dDataBase, 	 	Nil})
			aAdd(aTitulo,{"AUTHIST", 	  cHistBx, 	 	 	Nil})
		EndIf
		
		SE1->(dbSkip())
	End
	
	SE1->(dbCloseArea())
		
	If Len(aTitulo) > 0
		lMsErroAuto	:= .F.
		
		MSExecAuto({|x,y| Fina070(x,y)},aTitulo,3) // 3 - Baixa de Título, 5 - Cancelamento de baixa, 6 - Exclusão de Baixa.
		
		If lMsErroAuto
			MostraErro()
			Return()
		Else
			lOk := .T.
		EndIf
	EndIf
	
	RestArea(aArea)
Return(lOk)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! IMP5MH03	! Autor !Gilson Lima 		 ! Data ! 08/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Lê e importa o arquivo			 							 !
! 			! 5MH															 !
+----------------------------------------------------------------------------*/
Static Function IMP5MH03()

	Local lRet		:= .T.
	Local nLin		:= 0
	Local aDados	:= {}
	Local cQuery	:= ''
	
	Local cTpReg	:= ""	// Tipo de registro
	Local cProcess	:= "N"	// Registro Processado? (S/N)
	
	// Tipo de Registro = '0'
	Local cAnoMes	:= ""	// Ano e Mês de Referencia Proc. do PGN
	Local cSistRef	:= ""	// Sistema de Referência do Proc. do PGN
	Local cSeqProc	:= ""	// Sequencia do Processamento da Referência
	Local dDtProc	:= ""	// Data de Processamento do PGN (aaaammdd)
	Local cHrProc	:= ""	// Hora de Processamento do PGN (hhmm)
	Local cIdentif	:= ""	// Identificação de Responsável / Grupo de Usuários - PP = PRPREV / Beneficiarios Fundo de Previdência
	Local cBcos		:= ""	// Campo em Branco (ou Informações Adicionais)
	Local cCodOrc	:= ""	// Código do Orgao com 4 Dig
	
	// Tipo de Registro = '1'
	Local cCodDesc	:= ""	// Código do Desconto
	Local cNMDesc	:= ""	// Nome do Desconto
	Local cCodOrgao	:= ""	// Codigo do Orgao Interno ao Sistema RH
	Local cNMOrgao	:= ""	// Nome do Orgao em que foi descontado
	
	// Tipo de Registro = '2'
	Local nValor	:= 0	// Valor do Desconto
	Local cCPF		:= ""	// CPF do Servidor
	Local cRG		:= ""	// RG
	Local cNome		:= ""	// Nome
	Local cEvento	:= ""	// Tipo de Evento - "D" = Desconto / "E" = Estorno
	Local nNrVezes	:= 0	// Número de vezes do desconto (999 = Sempre)
	Local cBcos2	:= ""	// Espaço não utilizado ou outras informações	
	
	// Tipo de Registro = '3'
	Local nTotDesc	:= 0	// Total de Desconto
	Local nTotEst	:= 0	// Total de Estorno
	Local nTotCOpe	:= 0	// Total de Custo Operacional
	Local nTotCVar	:= 0	// Total de Custo Variável
	
	Local cRegProc	:= ''
	Local cCompet	:= ''
	Local lImp		:= .T.
	
	Local nTVlImp	:= 0
	Local nTRImp	:= 0
	
	// Verifica se existe o arquivo texto
	cFile := AllTrim(MV_PAR02)
	
	If !File(cFile)
		Alert("Arquivo não encontrado!")
		Return .F.
	EndIf
	
	// Abre o arquivo texto e grava dados
	FT_FUSE(cFile)
	FT_FGOTOP()
	
	// Conta os registros
	ProcRegua(RecCount())
	
	While !FT_FEOF()
	
		nLin++

		IncProc("Analisando Arquivo: Registro " + cValtoChar(nLin) + " ...")
	
		cLinha	:= FT_FREADLN()
		cLinha 	:= RetGraf(cLinha)
		
		// Vefifica se a linha contém informações e só então processa
		If Len(AllTrim(cLinha)) > 0
			
			cTpReg 	:= SubStr(cLinha,1,1)
			
			Do Case
				Case cTpReg == '0'

					cAnoMes		:= Substr(cLinha,2,6)		// Ano e Mês de Referencia Proc. do PGN
					cSistRef	:= Substr(cLinha,8,3)		// Sistema de Referência do Proc. do PGN
					cSeqProc	:= Substr(cLinha,11,2)		// Sequencia do Processamento da Referência
					dDtProc		:= StoD(Substr(cLinha,13,8))// Data de Processamento do PGN (aaaammdd)
					cHrProc		:= Substr(cLinha,21,4)		// Hora de Processamento do PGN (hhmm)
					cIdentif	:= Substr(cLinha,25,2)		// Identificação de Responsável / Grupo de Usuários - PP = PRPREV / Beneficiarios Fundo de Previdência
					cBcos		:= Substr(cLinha,27,9)		// Campo em Branco (ou Informações Adicionais)
					cCodOrc		:= Substr(cLinha,36,4)		// Código do Orgao com 4 Dig
					
				Case cTpReg == '1'
				
					cCodDesc	:= Substr(cLinha,2,3)		// Código do Desconto
					cNMDesc		:= Substr(cLinha,5,15)		// Nome do Desconto
					cCodOrgao	:= Substr(cLinha,20,2)		// Codigo do Orgao Interno ao Sistema RH
					cNMOrgao	:= Substr(cLinha,22,50)		// Nome do Orgao em que foi descontado
					
				Case cTpReg == '2'
					
					nValor		:= Val(SubStr(cLinha,5,13))/100	// Valor do Desconto
					cCPF		:= Substr(cLinha,18,13)		// CPF do Servidor
					cRG			:= Substr(cLinha,31,10)		// RG
					cNome		:= Substr(cLinha,41,25)		// Nome
					cEvento		:= Substr(cLinha,66,1)		// Tipo de Evento - "D" = Desconto / "E" = Estorno
					nNrVezes	:= Val(Substr(cLinha,67,3))	// Número de vezes do desconto (999 = Sempre)
					cBcos2		:= Substr(cLinha,70,4)		// Espaço não utilizado ou outras informações
					
					AADD(aDados,{;
						nLin		,;	// [1] Numero da Linha
						cAnoMes		,;	// [2] Ano e Mês de Referencia Proc. do PGN
						cSistRef	,;	// [3] Sistema de Referência do Proc. do PGN
						cSeqProc	,;	// [4] Sequencia do Processamento da Referência
						dDtProc		,;	// [5] Data de Processamento do PGN (aaaammdd)
						cHrProc		,;	// [6] Hora de Processamento do PGN (hhmm)
						cIdentif	,;	// [7] Identificação de Responsável / Grupo de Usuários - PP = PRPREV / Beneficiarios Fundo de Previdência
						cBcos		,;	// [8] Campo em Branco (ou Informações Adicionais)
						cCodOrc		,;	// [9] Código do Orgao com 4 Dig
						cCodDesc	,;	// [10] Código do Desconto
						cNMDesc		,;	// [11] Nome do Desconto
						cCodOrgao	,;	// [12] Codigo do Orgao Interno ao Sistema RH
						cNMOrgao	,;	// [13] Nome do Orgao em que foi descontado
						nValor		,;	// [14] Valor do Desconto
						cCPF		,;	// [15] CPF do Servidor
						cRG			,;	// [16] RG
						cNome		,;	// [17] Nome
						cEvento		,;	// [18] Tipo de Evento - "D" = Desconto / "E" = Estorno
						nNrVezes	,;	// [19] Número de vezes do desconto (999 = Sempre)
						cBcos2		,;	// [20] Espaço não utilizado ou outras informações	
						""			,;	// [21] nTotDesc - Total de Desconto
						""			,;	// [22] nTotEst - Total de Estorno
						""			,;	// [23] nTotCOpe - Total de Custo Operacional
						""			,;	// [24] nTotCVar - Total de Custo Variável
						dDataBase	;	// [25] Data de Importação
					})						
				
				Case cTpReg == '3'
				
					nTotDesc 	:= Val(SubStr(cLinha,5,13))/100		// Total de Desconto
					nTotEst 	:= Val(SubStr(cLinha,18,13))/100	// Total de Estorno
					nTotCOpe 	:= Val(SubStr(cLinha,31,13))/100	// Total de Custo Operacional
					nTotCVar 	:= Val(SubStr(cLinha,44,13))/100	// Total de Custo Variável
					
					For nX := 1 To Len(aDados)
						aDados[nx][21] := nTotDesc
						aDados[nx][22] := nTotEst
						aDados[nx][23] := nTotCOpe
						aDados[nx][24] := nTotCVar
					Next nX
								
			EndCase
		
		EndIf
		FT_FSKIP()
	EndDo
	
	// Fecha o arquivo texto
	FT_FUSE()
	
	// Verifica se Existem dados a gravar
	If Len(aDados) > 0
	
		// Verifica se Competência já foi importada anteriormente
		// Se já houver sido importada, verifica se já foi processada (rotina financeiro)
		// Se já houver sido processada, não permite re-importar, informando o usuário o motivo
		// Se não houver sido processada, informa ao usuário que já existe a competência no 
		// banco de dados e se deseja apagar os dados existentes e re-importar. Se Sim, apaga os registros existentes e re-importa o arquivo
		
		//cRegProc := POSICIONE('SZY',1,xFilial("SZY")+aDados[1][2],'ZY_PROCESS')
		
		cQuery	 := "SELECT Count(SZY.ZY_PROCESS) PROCESSADO "
		cQuery   += "FROM " + RetSqlName('SZY') + " SZY "
		cQuery   += "WHERE SZY.D_E_L_E_T_ = ' ' "
		cQuery   += "AND SZY.ZY_ANOMES = '" + aDados[1][2] + "' "
		cQuery   += "AND SZY.ZY_PROCESS = 'S' "
		cQuery	 += "AND SZY.ZY_IDENTIF = '" + aDados[1][7] + "'"
		
		If Select('SZY') <> 0
			dbSelectArea('SZY')
			SZY->(dbCloseArea())
		EndIf
		
		dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'SZY',.F.,.F.)
		
		dbSelectArea('SZY')
		SZY->(dbGoTop())
		
		nProcess := SZY->PROCESSADO
		
		SZY->(dbCloseArea())
		
		cCompet	 := Substr(aDados[1][2],5,2) + "/" + Substr(aDados[1][2],1,4)
		
		If nProcess > 0
			lImp := .F.
			Alert("Já existem registros importados e processados para a competência " + cCompet + " e por isso este arquivo não será importado!")
			Return
		Else
			cQuery	 := "SELECT Count(SZY.ZY_PROCESS) NPROCESSADO "
			cQuery   += "FROM " + RetSqlName('SZY') + " SZY "
			cQuery   += "WHERE SZY.D_E_L_E_T_ = ' ' "
			cQuery   += "AND SZY.ZY_ANOMES = '" + aDados[1][2] + "' "
			cQuery   += "AND SZY.ZY_PROCESS = 'N' "
			cQuery	 += "AND SZY.ZY_IDENTIF = '" + aDados[1][7] + "'"
			
			If Select('SZY') <> 0
				dbSelectArea('SZY')
				SZY->(dbCloseArea())
			EndIf
			
			dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'SZY',.F.,.F.)
			
			dbSelectArea('SZY')
			SZY->(dbGoTop())
			
			nNProcess := SZY->NPROCESSADO
			
			SZY->(dbCloseArea())
			
			If nNProcess > 0
		
				If MsgYesNo("Já existem registros importados para a competência " + cCompet + ". Deseja apagar os registros existentes e importá-los novamente?","Atenção")
					
					lImp := .T.
					
					dbSelectArea("SZY")
					SZY->(dbSetOrder(1))
					SZY->(dbGoTop())
					SZY->(dbSeek(xFilial("SZY")+aDados[1][2]))
			
					While SZY->(!EOF()) .And. (SZY->ZY_FILIAL+SZY->ZY_ANOMES == xFilial("SZY")+aDados[1][2])
						RecLock("SZY",.F.)
						SZY->(dbDelete())
						SZY->(MsUnlock())
						SZY->(dbSkip())
					End
				Else
					lImp := .F.
				EndIf
			EndIf
		EndIf
		
		// Caso não haja registros importados para essa competência ou o usuário deseje re-importar
		// Executa a rotina de importação
		
		If lImp
		
			nTRImp  := Len(aDados)
			nTVlImp := 0 
	
			For nY := 1 To Len(aDados)
			
				IncProc("Gravando Registro " + cValToChar(nY) + " ...")
				
				//Grava no Banco de Dados
				RecLock("SZY",.T.)
				
					SZY->ZY_FILIAL		:= xFilial("SZY")
					SZY->ZY_SEQUENC		:= aDados[nY][1]
					SZY->ZY_ANOMES		:= aDados[nY][2]
					SZY->ZY_SISTREF		:= aDados[nY][3]
					SZY->ZY_SEQPROC		:= aDados[nY][4]
					SZY->ZY_DTPROC		:= aDados[nY][5]
					SZY->ZY_HRPROC		:= aDados[nY][6]
					SZY->ZY_IDENTIF		:= aDados[nY][7]
					SZY->ZY_BCOS		:= aDados[nY][8]
					SZY->ZY_CODORCM		:= aDados[nY][9]
					SZY->ZY_CODDESC		:= aDados[nY][10]
					SZY->ZY_NMDESC		:= aDados[nY][11]
					SZY->ZY_CDORGAO		:= aDados[nY][12]
					SZY->ZY_NMORGAO		:= aDados[nY][13]
					SZY->ZY_VALOR		:= aDados[nY][14]
					SZY->ZY_CPF			:= aDados[nY][15]
					SZY->ZY_RG			:= aDados[nY][16]
					SZY->ZY_NOME		:= aDados[nY][17]
					SZY->ZY_EVENTO		:= aDados[nY][18]
					SZY->ZY_NRVEZES		:= aDados[nY][19]
					SZY->ZY_BCOS2		:= aDados[nY][20]
					SZY->ZY_TOTDESC		:= aDados[nY][21]
					SZY->ZY_TOTEST		:= aDados[nY][22]
					SZY->ZY_TOTCOPE		:= aDados[nY][23]
					SZY->ZY_TOTCVAR		:= aDados[nY][24]
					SZY->ZY_PROCESS		:= "N"
					SZY->ZY_DTIMP		:= aDados[nY][25]
				
				MsUnLock("SZY")
				
				nTVlImp += aDados[nY][14]
			Next nY
			
			Alert("Foram importados " + cValToChar(nTRImp) + " registros, totalizando R$: " + Transform(nTVlImp, "@E 99,999,999,999.99"))

		EndIf
	Else
		Alert('Nenhum registro importado!')
	EndIf
			
Return


/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RetGraf	! Autor !Gilson Lima 		 ! Data ! 15/10/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Função para retirar os Caracteres Especiais do texto			 !
!			!																 !
+----------------------------------------------------------------------------*/
Static Function RetGraf(_sOrig)

   local _sRet := _sOrig

   _sRet = StrTran (_sRet, "á", "a")
   _sRet = StrTran (_sRet, "é", "e")
   _sRet = StrTran (_sRet, "í", "i")
   _sRet = StrTran (_sRet, "ó", "o")
   _sRet = StrTran (_sRet, "ú", "u")
   _sRet = StrTran (_sRet, "Á", "A")
   _sRet = StrTran (_sRet, "É", "E")
   _sRet = StrTran (_sRet, "Í", "I")
   _sRet = StrTran (_sRet, "Ó", "O")
   _sRet = StrTran (_sRet, "Ú", "U")
   _sRet = StrTran (_sRet, "ã", "a")
   _sRet = StrTran (_sRet, "õ", "o")
   _sRet = StrTran (_sRet, "Ã", "A")
   _sRet = StrTran (_sRet, "Õ", "O")
   _sRet = StrTran (_sRet, "â", "a")
   _sRet = StrTran (_sRet, "ê", "e")
   _sRet = StrTran (_sRet, "î", "i")
   _sRet = StrTran (_sRet, "ô", "o")
   _sRet = StrTran (_sRet, "û", "u")
   _sRet = StrTran (_sRet, "Â", "A")
   _sRet = StrTran (_sRet, "Ê", "E")
   _sRet = StrTran (_sRet, "Î", "I")
   _sRet = StrTran (_sRet, "Ô", "O")
   _sRet = StrTran (_sRet, "Û", "U")
   _sRet = StrTran (_sRet, "ç", "c")
   _sRet = StrTran (_sRet, "Ç", "C")
   _sRet = StrTran (_sRet, "à", "a")
   _sRet = StrTran (_sRet, "À", "A")
   _sRet = StrTran (_sRet, "º", ".")
   _sRet = StrTran (_sRet, "ª", ".")
   _sRet = StrTran (_sRet, chr (9), " ") // TAB
   
Return _sRet