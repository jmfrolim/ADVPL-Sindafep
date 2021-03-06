/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Atualiza��o 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Financeiro 											 !
+------------------+---------------------------------------------------------+
!Nome 			   ! IMP5MH01 												 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Importa��o do arquivo 5MH de mensalidades descontadas	 !
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
! Descricao ! Funcao chamada no menu para a importa��o do arquivo			 !
! 			! 5MH - Mensalidades											 !
+----------------------------------------------------------------------------*/
User Function IMP5MH01()
	
	Private cFile	:= ""
	Private cPerg	:= "IMP5MH01"
	Private aPergs	:= {}
	
	// Cria pergunta caso n�o exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Importar 5MH?","","","mv_ch1","C",1,0,0,"C","","MV_PAR01","Sim","","","","","N�o","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Arquivo Importa��o","Arquivo Importa��o","Arquivo Importa��o","mv_ch2","C",60,0,0,"G","","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
		AADD(aPergs,{"Processar Baixas?","","","mv_ch3","C",1,0,0,"C","","MV_PAR03","Sim","","","","","N�o","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Banco","","","mv_ch4","C",20,0,0,"G","","MV_PAR04","","","","","","","","","","","","","","","","","","","","","","","","","SA65MH","","","",""})
		AADD(aPergs,{"Compet�ncia","","","mv_ch5","C",7,0,0,"G","","MV_PAR05","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 99/9999"})
		AADD(aPergs,{"Atualiza Vlr. Mensalidades","","","mv_ch6","C",1,0,0,"C","","MV_PAR06","Sim","","","","","N�o","","","","","","","","","","","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)
	
	// Chama a fun��o de importa��o
	If MsgYesNo("Confirma Processamento 5MH?","Aten��o")
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
	Local a5MH		:= {}
	Private aBanco	:= StrToKarr(MV_PAR04,'|')
	Private aLogs	:= {}
	Private nBaixas := 0
	Private nVlBaixas := 0
	
	Private lAtMensal := IIF(MV_PAR06 == 1, .T., .F.)

	// Se Importar arquivo = S, processa importa��o
	If MV_PAR01 == 1
		IMP5MH03()
	EndIf
	
	// Se Processar Baixas = S
	If MV_PAR03 == 1
	
		GrvLog("5MH - Processando Baixas")
		GrvLog("Data e Hora �nicio.: " + DtoC( Date() )  + " - " + Time() )
		GrvLog(Replicate('-',80))
	
		IncProc("Processando baixas ...")
		
		a5MH := {}
		cCompet	:= Substr(MV_PAR05,4,4) + Substr(MV_PAR05,1,2)
		
		dbSelectArea("SZY")
		SZY->(dbSetOrder(2))
		SZY->(dbGoTop())
		SZY->(dbSeek(xFilial("SZY")+cCompet))
		
		While SZY->(!EOF() .And. SZY->ZY_ANOMES == cCompet)
			
			// Executa apenas para os n�o processados
			If SZY->ZY_PROCESS == 'N'
			
				// Adicionar em um array os dados n�o processados, analizando se j� existe o cpf. Se sim, soma valor, se n�o, adiciona novo registro.
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
						aAdd(a5MH,{SZY->ZY_CPF, SZY->ZY_VALOR, SZY->ZY_IDENTIF})
					EndIf
				Else
					aAdd(a5MH,{SZY->ZY_CPF, SZY->ZY_VALOR, SZY->ZY_IDENTIF})
				EndIf

			EndIf
		
			SZY->(dbSkip())	
		End
		
		SZY->(dbCloseArea()) 
		
		// Processa Baixas e Atualiza��o de valores de mensalidades
		For nD := 1 To Len(a5MH)		
					
			// Se o retorno da baixa do t�tulo for positivo		
			If Baixa5MH(cCompet,a5MH[nD][1],a5MH[nD][2],a5MH[nD][3])
				
				dbSelectArea("SZY")
				SZY->(dbSetOrder(1))
				SZY->(dbGoTop())
				SZY->(dbSeek(xFilial("SZY")+cCompet+a5MH[nD][1]))
				
				While SZY->(!EOF()) .And. SZY->(ZY_ANOMES) == cCompet .And. SZY->(ZY_CPF) == a5MH[nD][1]
				
					// Grava informa��o de processado na tabela SZY				
					RecLock("SZY", .F.)
						SZY->ZY_PROCESS := 'S'
					SZY->(MsUnLock())
					
					SZY->(dbSkip())
				End
				
				SZY->(dbCloseArea())
			EndIf

			// Atualiza Valor da Mensalidade
			If lAtMensal
				
				cCpf	:= PadR(Substr(a5MH[nD][1],3), 14, ' ')
				
				// Busca C�digo do Cliente baseado no CPF
				cCodCli := POSICIONE("SA1",3,XFILIAL('SA1')+cCpf,"A1_COD")
				
				dbSelectArea('SA1')
				SA1->(dbSetOrder(3))
				SA1->(dbSeek(XFILIAL('SA1')+cCpf))
				
				// Atualiza o valor das mensalidades
				While SA1->(!EOF()) .And. SA1->A1_CGC == cCpf
					RecLock("SA1", .F.)
						SA1->A1_VLRMENS := a5MH[nD][2]
						// Atualiza forma de cobran�a (tempor�rio)
						// SA1->A1_FORMCOB := '2'
					SA1->(MsUnLock())
					SA1->(dbSkip())
				End
				
				SA1->(dbCloseArea())					
			EndIf
		Next nD
		
	EndIf
	
	GrvLog(Replicate('-',80))
	
	If nBaixas > 0
		GrvLog("T�tulos Baixados: " + cValToChar(nBaixas))
	EndIF
	
	If nVlBaixas > 0
		GrvLog("Valor Baixado...: " + Transform(nVlBaixas,"@E 999,999,999,999.99"))
	EndIf
	
	GrvLog("Processamento 5MH Conclu�do!")
	GrvLog("Data / Hora Conclus�o: " + DtoC( Date() )  + " / " + Time() )
	
	ExibeLog()
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! Baixa5MH	! Autor !Gilson Lima 		 ! Data ! 26/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Localiza t�tulo com as informa��es passadas por par�metros	 !
! 			! e efetua a baixa do mesmo, retornando se opera��o foi positiva !
+----------------------------------------------------------------------------*/
Static Function Baixa5MH(cCompet,cCPF,nValor,cOrig)
	
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
	Local lBaixado		:= .F.
	Local cTitInf		:= ""
	Local cOrigem		:= IIF(cOrig == 'ES', 'Secr. Est. Fazenda', 'Paran� Previd�ncia')
	
	// Ajusta CPF do arquivo 5MH
	cCPF 				:= PadR(Substr(cCPF,3), 14, ' ')

	// Busca C�digo do Cliente baseado no CPF
	cCodCli				:= POSICIONE("SA1",3,XFILIAL('SA1')+cCPF,"A1_COD")
	
	If Alltrim(cCodCli) == ''
		GrvLog("Erro >>> " + cOrigem + " >>> Cliente n�o encontrado: CPF " + cCPF)
		Return .F.
	EndIf
	
	dbSelectArea('SE1')
	SE1->(dbSetOrder(2))
	SE1->(dbGoTop())
	SE1->(dbSeek(XFILIAL("SE1")+cCodCli))
	
/*	If !Found()
		GrvLog("Erro >>> Nenhum T�tulo para o Cliente: CPF " + cCPF)
	EndIF*/

	While (SE1->(!EOF()) .And. SE1->E1_CLIENTE == cCodCli)
		If (SE1->E1_STATUS == 'A' .And. (SE1->E1_PREFIXO == 'MSL' .OR. SE1->E1_PREFIXO == 'MS1' .OR. SE1->E1_PREFIXO == 'MS2' .OR. SE1->E1_PREFIXO == 'MS3') .And. cHistTit $ SE1->E1_HIST)
			// Ajusta os valores do t�tulo conforme o recebido via 5MH
			If SE1->E1_VALOR != nValor
				RecLock("SE1", .F.)
					// Valor 5MH Maior que Valor do T�tulo
					If (nValor > SE1->E1_VALOR)
						nVlDifer := nValor - SE1->E1_VALOR
						
						SE1->E1_ACRESC  := nVlDifer
						SE1->E1_SDACRES := nVlDifer
						
					// Valor 5MH Menor que Valor do T�tulo
					ElseIf (nValor < SE1->E1_VALOR)
						nVlDifer := SE1->E1_VALOR - nValor
						
						SE1->E1_DECRESC  := nVlDifer
						SE1->E1_SDDECRE  := nVlDifer
					EndIf 
				SE1->(MsUnLock())
			EndIf
			
			// Adiciona os dados para baixa do t�tulo
			aAdd(aTitulo,{"E1_PREFIXO",   SE1->E1_PREFIXO, 	Nil})
			aAdd(aTitulo,{"E1_NUM", 	  SE1->E1_NUM, 	 	Nil})
			aAdd(aTitulo,{"E1_PARCELA",   SE1->E1_PARCELA,	Nil})
			aAdd(aTitulo,{"E1_TIPO", 	  SE1->E1_TIPO, 	Nil})
			aAdd(aTitulo,{"E1_CLIENTE",   SE1->E1_CLIENTE, 	Nil})
			aAdd(aTitulo,{"E1_LOJA", 	  SE1->E1_LOJA, 	Nil})
			aAdd(aTitulo,{"AUTJUROS", 	  0, 	 			Nil})
			aAdd(aTitulo,{"AUTMULTA", 	  0, 	 			Nil})
			aAdd(aTitulo,{"AUTVALREC", 	  nValor,			Nil})
			aAdd(aTitulo,{"AUTBANCO", 	  aBanco[1],		Nil})
			aAdd(aTitulo,{"AUTAGENCIA",	  aBanco[2],		Nil})
			aAdd(aTitulo,{"AUTCONTA",	  aBanco[3],		Nil})
			//aAdd(aTitulo,{"AUTMOTBX", 	  "NOR", 	 		Nil})
			aAdd(aTitulo,{"AUTMOTBX", 	  "5MH", 	 		Nil})
			aAdd(aTitulo,{"AUTDTBAIXA",   dDataBase, 	 	Nil})
			aAdd(aTitulo,{"AUTDTCREDITO", dDataBase, 	 	Nil})
			aAdd(aTitulo,{"AUTHIST", 	  cHistBx, 	 	 	Nil})
		ElseIf (SE1->E1_STATUS == 'B')
			lBaixado := .T.
			cTitInf := "Prefixo: " + SE1->E1_PREFIXO + " | Numero: " + SE1->E1_NUM + " | Parcela: " + SE1->E1_PARCELA
		EndIf
		
		SE1->(dbSkip())
	End
	
	SE1->(dbCloseArea())
		
	If (Len(aTitulo) > 0)
		lMsErroAuto	:= .F.
		
		MSExecAuto({|x,y| Fina070(x,y)}, aTitulo, 3) // 3 - Baixa de T�tulo, 5 - Cancelamento de baixa, 6 - Exclus�o de Baixa.
		
		If lMsErroAuto

            cNomArqErro := DtoS(Date())+"-"+Substr(Time(),1,2)+Substr(Time(),4,2)+"-tmp.log"
            cPath := '//'
            
            MostraErro(cPath, cNomArqErro)
            
            cMsgLog := MemoRead(cPath+cNomArqErro)
            
            GrvLog("Erro >>> " + cOrigem + " >>> Erro na Baixa do T�tulo: CPF" + cCPF)
            GrvLog(cMsgLog)
            
            MsErase(cNomArqErro)
            
			lOk := .F.
		Else
			lOk := .T.
			
			nBaixas++
			nVlBaixas += nValor
		EndIf
	Else
		If (lBaixado)
			GrvLog("Erro >>> " + cOrigem + " >>> T�tulo foi baixado anteriormente e encontra-se com Status igual a 'B'.")
			GrvLog("         Info. do Tit.: " + cTitInf )
		Else
			GrvLog("Erro >>> " + cOrigem + " >>> T�tulo n�o encontrado para o Cliente: CPF " + cCPF)
			GrvLog("         Compet�ncia: " + cMesAno )
		EndIf
	EndIf
	
	RestArea(aArea)
Return(lOk)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! IMP5MH03	! Autor !Gilson Lima 		 ! Data ! 08/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! L� e importa o arquivo			 							 !
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
	Local cAnoMes	:= ""	// Ano e M�s de Referencia Proc. do PGN
	Local cSistRef	:= ""	// Sistema de Refer�ncia do Proc. do PGN
	Local cSeqProc	:= ""	// Sequencia do Processamento da Refer�ncia
	Local dDtProc	:= ""	// Data de Processamento do PGN (aaaammdd)
	Local cHrProc	:= ""	// Hora de Processamento do PGN (hhmm)
	Local cIdentif	:= ""	// Identifica��o de Respons�vel / Grupo de Usu�rios - PP = PRPREV / Beneficiarios Fundo de Previd�ncia
	Local cBcos		:= ""	// Campo em Branco (ou Informa��es Adicionais)
	Local cCodOrc	:= ""	// C�digo do Orgao com 4 Dig
	
	// Tipo de Registro = '1'
	Local cCodDesc	:= ""	// C�digo do Desconto
	Local cNMDesc	:= ""	// Nome do Desconto
	Local cCodOrgao	:= ""	// Codigo do Orgao Interno ao Sistema RH
	Local cNMOrgao	:= ""	// Nome do Orgao em que foi descontado
	
	// Tipo de Registro = '2'
	Local nValor	:= 0	// Valor do Desconto
	Local cCPF		:= ""	// CPF do Servidor
	Local cRG		:= ""	// RG
	Local cNome		:= ""	// Nome
	Local cEvento	:= ""	// Tipo de Evento - "D" = Desconto / "E" = Estorno
	Local nNrVezes	:= 0	// N�mero de vezes do desconto (999 = Sempre)
	Local cBcos2	:= ""	// Espa�o n�o utilizado ou outras informa��es	
	
	// Tipo de Registro = '3'
	Local nTotDesc	:= 0	// Total de Desconto
	Local nTotEst	:= 0	// Total de Estorno
	Local nTotCOpe	:= 0	// Total de Custo Operacional
	Local nTotCVar	:= 0	// Total de Custo Vari�vel
	
	Local cRegProc	:= ''
	Local cCompet	:= ''
	Local lImp		:= .T.
	
	Local nTVlImp	:= 0
	Local nTRImp	:= 0
	
	GrvLog("5MH - Importa��o de Arquivo")
	GrvLog("Data e Hora �nicio.: " + DtoC( Date() )  + " - " + Time() )
	GrvLog(Replicate('-',80))
	
	// Verifica se existe o arquivo texto
	cFile := AllTrim(MV_PAR02)
	
	If !File(cFile)
		GrvLog("Erro >>> Arquivo n�o encontrado: "+cFile)
		Alert("Arquivo n�o encontrado!")
		Return .F.
	EndIf
	
	// Abre o arquivo texto e grava dados
	FT_FUSE(cFile)
	FT_FGOTOP()
	
	// Conta os registros
	ProcRegua(RecCount())
	
	GrvLog("Info >>> Iniciado processo de Leitura")
	
	While !FT_FEOF()
	
		nLin++

		IncProc("Analisando Arquivo: Registro " + cValtoChar(nLin) + " ...")
	
		cLinha	:= FT_FREADLN()
		cLinha 	:= RetGraf(cLinha)
		
		// Vefifica se a linha cont�m informa��es e s� ent�o processa
		If Len(AllTrim(cLinha)) > 0
			
			cTpReg 	:= SubStr(cLinha,1,1)
			
			Do Case
				Case cTpReg == '0'

					cAnoMes		:= Substr(cLinha,2,6)		// Ano e M�s de Referencia Proc. do PGN
					cSistRef	:= Substr(cLinha,8,3)		// Sistema de Refer�ncia do Proc. do PGN
					cSeqProc	:= Substr(cLinha,11,2)		// Sequencia do Processamento da Refer�ncia
					dDtProc		:= StoD(Substr(cLinha,13,8))// Data de Processamento do PGN (aaaammdd)
					cHrProc		:= Substr(cLinha,21,4)		// Hora de Processamento do PGN (hhmm)
					cIdentif	:= Substr(cLinha,25,2)		// Identifica��o de Respons�vel / Grupo de Usu�rios - PP = PRPREV / Beneficiarios Fundo de Previd�ncia
					cBcos		:= Substr(cLinha,27,9)		// Campo em Branco (ou Informa��es Adicionais)
					cCodOrc		:= Substr(cLinha,36,4)		// C�digo do Orgao com 4 Dig
					
				Case cTpReg == '1'
				
					cCodDesc	:= Substr(cLinha,2,3)		// C�digo do Desconto
					cNMDesc		:= Substr(cLinha,5,15)		// Nome do Desconto
					cCodOrgao	:= Substr(cLinha,20,2)		// Codigo do Orgao Interno ao Sistema RH
					cNMOrgao	:= Substr(cLinha,22,50)		// Nome do Orgao em que foi descontado
					
				Case cTpReg == '2'
					
					nValor		:= Val(SubStr(cLinha,5,13))/100	// Valor do Desconto
					cCPF		:= Substr(cLinha,18,13)		// CPF do Servidor
					cRG			:= Substr(cLinha,31,10)		// RG
					cNome		:= Substr(cLinha,41,25)		// Nome
					cEvento		:= Substr(cLinha,66,1)		// Tipo de Evento - "D" = Desconto / "E" = Estorno
					nNrVezes	:= Val(Substr(cLinha,67,3))	// N�mero de vezes do desconto (999 = Sempre)
					cBcos2		:= Substr(cLinha,70,4)		// Espa�o n�o utilizado ou outras informa��es
					
					AADD(aDados,{;
						nLin		,;	// [1] Numero da Linha
						cAnoMes		,;	// [2] Ano e M�s de Referencia Proc. do PGN
						cSistRef	,;	// [3] Sistema de Refer�ncia do Proc. do PGN
						cSeqProc	,;	// [4] Sequencia do Processamento da Refer�ncia
						dDtProc		,;	// [5] Data de Processamento do PGN (aaaammdd)
						cHrProc		,;	// [6] Hora de Processamento do PGN (hhmm)
						cIdentif	,;	// [7] Identifica��o de Respons�vel / Grupo de Usu�rios - PP = PRPREV / Beneficiarios Fundo de Previd�ncia
						cBcos		,;	// [8] Campo em Branco (ou Informa��es Adicionais)
						cCodOrc		,;	// [9] C�digo do Orgao com 4 Dig
						cCodDesc	,;	// [10] C�digo do Desconto
						cNMDesc		,;	// [11] Nome do Desconto
						cCodOrgao	,;	// [12] Codigo do Orgao Interno ao Sistema RH
						cNMOrgao	,;	// [13] Nome do Orgao em que foi descontado
						nValor		,;	// [14] Valor do Desconto
						cCPF		,;	// [15] CPF do Servidor
						cRG			,;	// [16] RG
						cNome		,;	// [17] Nome
						cEvento		,;	// [18] Tipo de Evento - "D" = Desconto / "E" = Estorno
						nNrVezes	,;	// [19] N�mero de vezes do desconto (999 = Sempre)
						cBcos2		,;	// [20] Espa�o n�o utilizado ou outras informa��es	
						""			,;	// [21] nTotDesc - Total de Desconto
						""			,;	// [22] nTotEst - Total de Estorno
						""			,;	// [23] nTotCOpe - Total de Custo Operacional
						""			,;	// [24] nTotCVar - Total de Custo Vari�vel
						dDataBase	;	// [25] Data de Importa��o
					})						
				
				Case cTpReg == '3'
				
					nTotDesc 	:= Val(SubStr(cLinha,5,13))/100		// Total de Desconto
					nTotEst 	:= Val(SubStr(cLinha,18,13))/100	// Total de Estorno
					nTotCOpe 	:= Val(SubStr(cLinha,31,13))/100	// Total de Custo Operacional
					nTotCVar 	:= Val(SubStr(cLinha,44,13))/100	// Total de Custo Vari�vel
					
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
	
		GrvLog("Info >>> Iniciado processo de Grava��o")
	
		// Verifica se Compet�ncia j� foi importada anteriormente
		// Se j� houver sido importada, verifica se j� foi processada (rotina financeiro)
		// Se j� houver sido processada, n�o permite re-importar, informando o usu�rio o motivo
		// Se n�o houver sido processada, informa ao usu�rio que j� existe a compet�ncia no 
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
			
			GrvLog("Alert >> J� existem registros importados e processados para a compet�ncia " + cCompet)
			GrvLog("         e por isso este arquivo n�o ser� importado!")
			
			Alert("J� existem registros importados e processados para a compet�ncia " + cCompet + " e por isso este arquivo n�o ser� importado!")
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
		
				If MsgYesNo("J� existem registros importados para a compet�ncia " + cCompet + ". Deseja apagar os registros existentes e import�-los novamente?","Aten��o")
					
					GrvLog("Alert >> J� existem registros importados para a compet�ncia " + cCompet)
					GrvLog("         Ser�o apagados e importados novamente!")
			
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
		
		// Caso n�o haja registros importados para essa compet�ncia ou o usu�rio deseje re-importar
		// Executa a rotina de importa��o
		
		If lImp
		
			GrvLog("Iniciando Importa��o")
			
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
			
			GrvLog("Importa��o 5MH Conclu�da")
			GrvLog("Data / Hora Conclus�o: " + DtoC( Date() )  + " / " + Time() )
			GrvLog("Registros Importados.: " + cValToChar(nTRImp))
			GrvLog("Total R$.............: " + Transform(nTVlImp, "@E 99,999,999,999.99"))
			GrvLog(Replicate('-',80))
		
			Aviso("Aviso", "Foram importados " + cValToChar(nTRImp) + " registros, totalizando R$: " + Transform(nTVlImp, "@E 99,999,999,999.99"), {"Confirmar"}, 2)

		EndIf
	Else
		GrvLog("Importa��o 5MH Conclu�da")
		GrvLog("Data / Hora Conclus�o: " + DtoC( Date() )  + " / " + Time() )
		GrvLog("Registros Importados.: 0")
		GrvLog(Replicate('-',80))

		Alert('Nenhum registro importado!')
	EndIf
			
Return


/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RetGraf	! Autor !Gilson Lima 		 ! Data ! 15/10/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Fun��o para retirar os Caracteres Especiais do texto			 !
!			!																 !
+----------------------------------------------------------------------------*/
Static Function RetGraf(_sOrig)

   local _sRet := _sOrig

   _sRet = StrTran (_sRet, "�", "a")
   _sRet = StrTran (_sRet, "�", "e")
   _sRet = StrTran (_sRet, "�", "i")
   _sRet = StrTran (_sRet, "�", "o")
   _sRet = StrTran (_sRet, "�", "u")
   _sRet = StrTran (_sRet, "�", "A")
   _sRet = StrTran (_sRet, "�", "E")
   _sRet = StrTran (_sRet, "�", "I")
   _sRet = StrTran (_sRet, "�", "O")
   _sRet = StrTran (_sRet, "�", "U")
   _sRet = StrTran (_sRet, "�", "a")
   _sRet = StrTran (_sRet, "�", "o")
   _sRet = StrTran (_sRet, "�", "A")
   _sRet = StrTran (_sRet, "�", "O")
   _sRet = StrTran (_sRet, "�", "a")
   _sRet = StrTran (_sRet, "�", "e")
   _sRet = StrTran (_sRet, "�", "i")
   _sRet = StrTran (_sRet, "�", "o")
   _sRet = StrTran (_sRet, "�", "u")
   _sRet = StrTran (_sRet, "�", "A")
   _sRet = StrTran (_sRet, "�", "E")
   _sRet = StrTran (_sRet, "�", "I")
   _sRet = StrTran (_sRet, "�", "O")
   _sRet = StrTran (_sRet, "�", "U")
   _sRet = StrTran (_sRet, "�", "c")
   _sRet = StrTran (_sRet, "�", "C")
   _sRet = StrTran (_sRet, "�", "a")
   _sRet = StrTran (_sRet, "�", "A")
   _sRet = StrTran (_sRet, "�", ".")
   _sRet = StrTran (_sRet, "�", ".")
   _sRet = StrTran (_sRet, chr (9), " ") // TAB
   
Return _sRet

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GrvLog	! Autor !Gilson Lima 		 ! Data ! 17/03/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao auxiliar de adi��o da mensagem de log no array de Logs	 !
! 			! de sucesso e erros da rotina									 !
+----------------------------------------------------------------------------*/
Static Function GrvLog(cLog)

	aAdd(aLogs,cLog)

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! ExibeLog	! Autor !Gilson Lima 		 ! Data ! 17/03/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada ao final do processamento para exibi��o do LOG	 !
! 			! de sucesso e erros da rotina									 !
+----------------------------------------------------------------------------*/
Static Function ExibeLog()

	Local cMask     := "Arquivos Texto" + "(*.TXT)|*.txt|"
	Local cTexto	:= ''
	
	Local   cFile     := ""
	Local   cFileLog  := ""
	Local   oDlg      := NIL
	Local   oFont     := NIL
	Local   oMemo     := NIL	
	
	cTexto := LeLog()

	Define Font oFont Name "Courier New" Size 8, 18

	Define MsDialog oDlg Title "PROCESSAMENTO 5MH" From 3, 0 to 450, 673 Pixel

	@ 5, 5 Get oMemo Var cTexto Memo Size 330, 200 Of oDlg Pixel
	oMemo:bRClicked := { || AllwaysTrue() }
	oMemo:oFont     := oFont

	Define SButton From 209, 309 Type  1 Action oDlg:End() Enable Of oDlg Pixel // Apaga
	Define SButton From 209, 279 Type 13 Action ( cFile := cGetFile( cMask, "" ), If( cFile == "", .T., ;
	MemoWrite( cFile, cTexto ) ) ) Enable Of oDlg Pixel

	Activate MsDialog oDlg Center

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! LeLog		! Autor !Gilson Lima 		 ! Data ! 17/03/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao que efetua a leitura do array de LOG do sistema e 	 	 !
! 			! retorna em vari�vel para exibi��o em tela						 !
+----------------------------------------------------------------------------*/
Static Function LeLog()

/*	Local cRet  := ""
	Local cFile := NomeAutoLog()
	Local cAux  := ""
	
	FT_FUSE( cFile )
	FT_FGOTOP()
	
	While !FT_FEOF()
	
		cAux := FT_FREADLN()
	
		If Len( cRet ) + Len( cAux ) < 1048000
			cRet += cAux + CRLF
		Else
			cRet += CRLF
			cRet += Replicate( "=" , 128 ) + CRLF
			cRet += "Tamanho de exibi��o maxima do LOG alcan�ado." + CRLF
			cRet += "LOG Completo no arquivo " + cFile + CRLF
			cRet += Replicate( "=" , 128 ) + CRLF
			Exit
		EndIf
	
		FT_FSKIP()
	End
	
	FT_FUSE()
*/

	Local cRet := ''
	
	For nA := 1 To Len(aLogs)
		cRet += aLogs[nA] + CRLF
	Next nA
		
Return cRet
