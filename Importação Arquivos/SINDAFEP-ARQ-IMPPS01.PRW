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
!Nome 			   ! IMPPS01 												 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Importa��o do arquivo de Co-participa��o do Plano de	 !
!				   ! Sa�de Unimed 									 		 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 10/11/14 												 !
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
! Programa 	! IMPPS01	! Autor !Gilson Lima 		 ! Data ! 10/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada no menu para a importa��o do arquivo			 !
! 			! de Co-participacao do Plano de Saude Unimed					 !
+----------------------------------------------------------------------------*/
User Function IMPPS01()
	
	Private cFile	:= ""
	Private cPerg	:= "IMPPS01"
	Private aPergs	:= {}
	
	// Cria pergunta caso n�o exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Arquivo Importa��o","Arquivo Importa��o","Arquivo Importa��o","mv_ch1","C",60,0,0,"G","","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)
	
	// Chama a fun��o de importa��o
	If MsgYesNo("Confirma importa��o do arquivo?","Aten��o")
		Processa({||IMPPS02()})
	EndIf
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! IMPPS02	! Autor !Gilson Lima 		 ! Data ! 10/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! L� e importa o arquivo			 							 !
! 			! de Co-participacao do Plano de Saude Unimed					 !
+----------------------------------------------------------------------------*/
Static Function IMPPS02()

	Local lRet		:= .T.
	Local nLin		:= 0
	Local aDados	:= {}
	
	Local cSeqReg	:= ""	// N�mero sequencial do registro
	Local cTpReg	:= ""	// Tipo de registro
	
	// Tipo de Registro = 'H'
	Local cNmSing	:= ""	// Nome da Singular Emissora do Arquivo		
	Local cEmpArq	:= ""	// Empresa para qual foi emitido o arquivo (contrato)
	Local cCodContr	:= ""	// C�digo do Contrato
	Local cCompFat	:= ""	// Compet�ncia de Faturamento
	Local cCodRel	:= ""	// C�digo do Relat�rio
	
	// Tipo de Registro = 'C'
	Local cNmContr	:= ""	// Nome do contratante dos registros seguintes
	
	// Tipo de Registro = 'F'
	Local cNmResp	:= ""	// Nome do respons�vel  da fam�lia dos pr�ximos registros de dependentes (D)
	Local cFamilia	:= ""	// C�digo da Fam�lia
	
	// Tipo de Registro = 'D'
	Local cCodBenef	:= "" 	// C�digo do benefici�rio
	Local cNmBenef	:= ""	// Nome do benefici�rio
	Local dDtNBenef	:= ""	// Data de Nascimento do benefici�rio
	Local cSexBenef	:= ""	// Sexo do Benefici�rio
	Local cNotaServ	:= ""	// Nr. da nota emitida ref. ao servi�o
	Local dDtAtend	:= ""	// Data de Atendimento
	Local dDtInter	:= ""	// Data de Interna��o
	Local dDtAlta	:= ""	// Data de Alta
	Local cCodReceb	:= ""	// C�digo do Recebedor
	Local cDescGrau	:= ""	// Descri��o do Grau
	Local cEmerg	:= ""	// Emerg�ncia S/N = Branco
	Local cHrAtend	:= ""	// Hora de Atendimento
	Local cCodAcom	:= ""	// C�digo do Tipo de Acomoda��o
	Local cCodGrau	:= ""	// C�digo do Grau
	Local cCodServ	:= ""	// C�digo do Servi�o dentro do Grupo
	Local nQtdServ	:= 0	// Quantidade de servi�o executado
	Local cSigCons	:= ""	// Sigla do Conselho - Profissional
	Local cNrCons	:= ""	// Nr. do Conselho - Profissional
	Local cUFCons	:= ""	// UF do Conselho - Profissional
	Local nVlCusto	:= 0	// Valor do Custo Operacional
	Local nVlFilme	:= 0	// Valor do Filme
	Local nVlHonMed	:= 0	// Valor dos honor�rios m�dicos
	Local nTxAdmin	:= 0	// Taxa Administrativa
	Local nVlServ	:= 0	// Valor cobrado do servi�o
	Local nVlBsINSS	:= 0	// Valor da base do INSS
	Local cMatrEmp	:= ""	// Matricula do benefici�rio na empresa contratante
	Local cCodPrest	:= ""	// C�digo do prestador executor
	Local cNmPrest	:= ""	// Nome do prestador executor
	Local cEspecial	:= ""	// Especialidade
	Local cCidPrinc	:= ""	// Cid Principal
	Local cGrauDep	:= ""	// Grau de Depend�ncia
	Local cCodProc	:= ""	// C�digo do procedimento utilizado no atendimento
	Local cDescProc	:= ""	// Descri��o do Procedimento
	Local cRegAtend	:= ""	// Regime de atendimento
	Local cCPF		:= ""	// CPF
	Local cDevCob	:= ""	// Devolu��o ou Cobran�a
	Local nQtdReali	:= 0	// Quantidade Realizada
	
	// Tipo de Registro = 'R'
	Local nVlTotSer := 0	// Valor total dos servi�os
	
	// Tipo de Registro = "T"
	Local nVlTtServ := 0	// Valor total dos servi�os
	Local nQtdReg	:= 0	// Quantidade de registros no arquivo
		
	Local cRegProc	:= ''
	Local cCompet	:= ''
	Local lImp		:= .T.
	
	Local nTVlImp	:= 0
	Local nTRImp	:= 0	
	
	// Verifica se existe o arquivo texto
	cFile := AllTrim(MV_PAR01)
	
	If !File(cFile)
		Alert("Arquivo n�o encontrado!")
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
		
		// Vefifica se a linha cont�m informa��es e s� ent�o processa
		If Len(AllTrim(cLinha)) > 0
			
			cSeqReg	:= SubStr(cLinha,1,6)
			cTpReg 	:= SubStr(cLinha,7,1)
			
			Do Case
				Case cTpReg == 'H'
					
					cNmSing		:= Substr(cLinha,8,30)	// Nome da Singular Emissora do Arquivo		
					cEmpArq		:= Substr(cLinha,38,30)	// Empresa para qual foi emitido o arquivo (contrato)
					cCodContr	:= Substr(cLinha,69,10)	// C�digo do Contrato
					cCompFat	:= Substr(cLinha,81,4) + Substr(cLinha,79,2)	// Compet�ncia de Faturamento
					cCodRel		:= Substr(cLinha,85,7)	// C�digo do Relat�rio
					
				Case cTpReg == 'C'
				
					cNmContr	:= SubStr(cLinha,8,30)	// Nome do contratante dos registros seguintes
				
				Case cTpReg == 'F'
					
					cNmResp		:= SubStr(cLinha,8,30)	// Nome do respons�vel da fam�lia dos pr�ximos registros de dependentes (D)
					cFamilia	:= SubStr(cLinha,38,7)	// C�digo da Fam�lia
					
				Case cTpReg == 'D'
				
					cCodBenef	:= SubStr(cLinha,8,13) 	// C�digo do benefici�rio
					cNmBenef	:= SubStr(cLinha,21,25)	// Nome do benefici�rio
					dDtNBenef	:= StoD(SubStr(cLinha,50,4) + Substr(cLinha,48,2) + Substr(cLinha,46,2))	// Data de Nascimento do benefici�rio
					cSexBenef	:= SubStr(cLinha,54,1)	// Sexo do Benefici�rio
					cNotaServ	:= SubStr(cLinha,55,20)	// Nr. da nota emitida ref. ao servi�o
					dDtAtend	:= StoD(SubStr(cLinha,79,4) + Substr(cLinha,77,2) + Substr(cLinha,75,2))	// Data de Atendimento
					dDtInter	:= StoD(SubStr(cLinha,87,4) + Substr(cLinha,85,2) + Substr(cLinha,83,2))	// Data de Interna��o
					dDtAlta		:= StoD(SubStr(cLinha,95,4) + Substr(cLinha,93,2) + Substr(cLinha,91,2))	// Data de Alta
					cCodReceb	:= SubStr(cLinha,99,7)	// C�digo do Recebedor
					cDescGrau	:= SubStr(cLinha,106,10)// Descri��o do Grau
					cEmerg		:= SubStr(cLinha,116,1)	// Emerg�ncia S/N = Branco
					cHrAtend	:= SubStr(cLinha,117,4)	// Hora de Atendimento
					cCodAcom	:= SubStr(cLinha,121,2)	// C�digo do Tipo de Acomoda��o
					cCodGrau	:= SubStr(cLinha,123,3)	// C�digo do Grau
					cCodServ	:= SubStr(cLinha,126,8)	// C�digo do Servi�o dentro do Grupo
					nQtdServ	:= Val(SubStr(cLinha,134,11))	// Quantidade de servi�o executado
					cSigCons	:= SubStr(cLinha,145,12)// Sigla do Conselho - Profissional
					cNrCons		:= SubStr(cLinha,157,7)	// Nr. do Conselho - Profissional
					cUFCons		:= SubStr(cLinha,164,2)	// UF do Conselho - Profissional
					nVlCusto	:= Val(SubStr(cLinha,166,13))/100 	// Valor do Custo Operacional
					nVlFilme	:= Val(SubStr(cLinha,179,13))/100 	// Valor do Filme
					nVlHonMed	:= Val(SubStr(cLinha,192,13))/100 	// Valor dos honor�rios m�dicos
					nTxAdmin	:= Val(SubStr(cLinha,205,13))/100	// Taxa Administrativa
					nVlServ		:= Val(SubStr(cLinha,218,13))/100	// Valor cobrado do servi�o
					nVlBsINSS	:= Val(SubStr(cLinha,231,13))/100	// Valor da base do INSS
					cMatrEmp	:= SubStr(cLinha,244,17)// Matricula do benefici�rio na empresa contratante
					cCodPrest	:= SubStr(cLinha,261,8)	// C�digo do prestador executor
					cNmPrest	:= SubStr(cLinha,269,40)// Nome do prestador executor
					cEspecial	:= SubStr(cLinha,309,22)// Especialidade
					cCidPrinc	:= SubStr(cLinha,331,6)	// Cid Principal
					cGrauDep	:= SubStr(cLinha,337,2)	// Grau de Depend�ncia
					cCodProc	:= SubStr(cLinha,339,8)	// C�digo do procedimento utilizado no atendimento
					cDescProc	:= SubStr(cLinha,347,80)// Descri��o do Procedimento
					cRegAtend	:= SubStr(cLinha,427,20)// Regime de atendimento
					cCPF		:= SubStr(cLinha,447,11)// CPF
					cDevCob		:= SubStr(cLinha,458,1)	// Devolu��o ou Cobran�a
					nQtdReali	:= Val(SubStr(cLinha,459,6))		// Quantidade Realizada
					
					AADD(aDados,{;
						cSeqReg,;	// [1] N�mero sequencial do registro
						cTpReg,;		// [2] Tipo de registro
						cNmSing,;	// [3] Nome da Singular Emissora do Arquivo		
						cEmpArq,;	// [4] Empresa para qual foi emitido o arquivo (contrato)
						cCodContr,;	// [5] C�digo do Contrato
						cCompFat,;	// [6] Compet�ncia de Faturamento
						cCodRel,;	// [7] C�digo do Relat�rio
						cNmContr,;	// [8] Nome do contratante dos registros seguintes
						cNmResp,;	// [9] Nome do respons�vel  da fam�lia dos pr�ximos registros de dependentes (D)
						cFamilia,;	// [10] C�digo da Fam�lia
						cCodBenef,;	// [11] C�digo do benefici�rio
						cNmBenef,;	// [12] Nome do benefici�rio
						dDtNBenef,;	// [13] Data de Nascimento do benefici�rio
						cSexBenef,;	// [14] Sexo do Benefici�rio
						cNotaServ,;	// [15] Nr. da nota emitida ref. ao servi�o
						dDtAtend,;	// [16] Data de Atendimento
						dDtInter,;	// [17] Data de Interna��o
						dDtAlta,;	// [18] Data de Alta
						cCodReceb,;	// [19] C�digo do Recebedor
						cDescGrau,;	// [20] Descri��o do Grau
						cEmerg,;		// [21] Emerg�ncia S/N = Branco
						cHrAtend,;	// [22] Hora de Atendimento
						cCodAcom,;	// [23] C�digo do Tipo de Acomoda��o
						cCodGrau,;	// [24] C�digo do Grau
						cCodServ,;	// [25] C�digo do Servi�o dentro do Grupo
						nQtdServ,;	// [26] Quantidade de servi�o executado
						cSigCons,;	// [27] Sigla do Conselho - Profissional
						cNrCons,;	// [28] Nr. do Conselho - Profissional
						cUFCons,;	// [29] UF do Conselho - Profissional
						nVlCusto,;	// [30] Valor do Custo Operacional
						nVlFilme,;	// [31] Valor do Filme
						nVlHonMed,;	// [32] Valor dos honor�rios m�dicos
						nTxAdmin,;	// [33] Taxa Administrativa
						nVlServ,;	// [34] Valor cobrado do servi�o
						nVlBsINSS,;	// [35] Valor da base do INSS
						cMatrEmp,;	// [36] Matricula do benefici�rio na empresa contratante
						cCodPrest,;	// [37] C�digo do prestador executor
						cNmPrest,;	// [38] Nome do prestador executor
						cEspecial,;	// [39] Especialidade
						cCidPrinc,;	// [40] Cid Principal
						cGrauDep,;	// [41] Grau de Depend�ncia
						cCodProc,;	// [42] C�digo do procedimento utilizado no atendimento
						cDescProc,;	// [43] Descri��o do Procedimento
						cRegAtend,;	// [44] Regime de atendimento
						cCPF,;		// [45] CPF
						cDevCob,;	// [46] Devolu��o ou Cobran�a
						nQtdReali,;	// [47] Quantidade Realizada
						"",;			// [48] nVlTotSer - Valor total dos servi�os
						"",;			// [49] nVlTtServ - Valor total dos servi�os
						"",;			// [50] nQtdReg - Quantidade de registros no arquivo
						dDataBase;	// [51] Data de Importa��o
					})
				
				Case cTpReg == 'R'
				
					nVlTotSer 	:= Val(SubStr(cLinha,8,14))/100	// Valor total dos servi�os
				
				Case cTpReg == 'T'
				
					nVlTtServ 	:= Val(SubStr(cLinha,8,14))/100	// Valor total dos servi�os
					nQtdReg		:= Val(SubStr(cLinha,22,7))		// Quantidade de registros no arquivo
					
					For nX := 1 To Len(aDados)
						aDados[nx][48] := nVlTotSer
						aDados[nx][49] := nVlTtServ
						aDados[nx][50] := nQtdReg
					Next nX
								
			EndCase
		
		EndIf
		FT_FSKIP()
	EndDo
	
	// Fecha o arquivo texto
	FT_FUSE()
	
	// Verifica se Existem dados a gravar
	If Len(aDados) > 0

		// Verifica se Compet�ncia j� foi importada anteriormente
		// Se j� houver sido importada, verifica se j� foi processada (rotina financeiro)
		// Se j� houver sido processada, n�o permite re-importar, informando o usu�rio o motivo
		// Se n�o houver sido processada, informa ao usu�rio que j� existe a compet�ncia no 
		// banco de dados e se deseja apagar os dados existentes e re-importar. Se Sim, apaga os registros existentes e re-importa o arquivo
		
		cRegProc := POSICIONE('SZ1',2,xFilial("SZ1")+aDados[1][6],'Z1_PROCESS')	
		cCompet	 := Substr(aDados[1][6],5,2) + "/" + Substr(aDados[1][6],1,4)
		
		If Len(AllTrim(cRegProc)) > 0
			If cRegProc == "S"
				lImp := .F.
				Alert("J� existem registros importados e processados para a compet�ncia " + cCompet + " e por isso este arquivo n�o ser� importado!")
				Return
			Else
				If MsgYesNo("J� existem registros importados para a compet�ncia " + cCompet + ". Deseja apagar os registros existentes e import�-los novamente?","Aten��o")
					
					lImp := .T.
					
					dbSelectArea("SZ1")
					SZ1->(dbSetOrder(2))
					SZ1->(dbGoTop())
					SZ1->(dbSeek(xFilial("SZY")+aDados[1][6]))
			
					While SZY->(!EOF()) .And. (SZ1->Z1_FILIAL+SZ1->Z1_COMPFAT == xFilial("SZ1")+aDados[1][6])
						RecLock("SZ1",.F.)
						SZ1->(dbDelete())
						SZ1->(MsUnlock())
						SZ1->(dbSkip())
					End
				Else
					lImp := .F.
				EndIf
			EndIf
		EndIf
		
		// Caso n�o haja registros importados para essa compet�ncia ou o usu�rio deseje re-importar
		// Executa a rotina de importa��o
		
		If lImp

			nTRImp  := Len(aDados)
			nTVlImp := 0 	
	
			For nY := 1 To Len(aDados)
			
				IncProc("Gravando Registro " + cValToChar(nY) + " ...")
				
				//Grava no Banco de Dados
				RecLock("SZ1",.T.)
				
					SZ1->Z1_FILIAL		:= xFilial("SZ1")
					SZ1->Z1_SEQREG		:= aDados[nY][1]
					SZ1->Z1_TPREG		:= aDados[nY][2]
					SZ1->Z1_NMSING		:= aDados[nY][3]
					SZ1->Z1_EMPARQ		:= aDados[nY][4]
					SZ1->Z1_CDCONTR		:= aDados[nY][5]
					SZ1->Z1_COMPFAT		:= aDados[nY][6]
					SZ1->Z1_CODREL		:= aDados[nY][7]
					SZ1->Z1_NMCONTR		:= aDados[nY][8]
					SZ1->Z1_NMRESP		:= aDados[nY][9]
					SZ1->Z1_FAMILIA		:= aDados[nY][10]
					SZ1->Z1_CDBENEF		:= aDados[nY][11]
					SZ1->Z1_NMBENEF		:= aDados[nY][12]
					SZ1->Z1_DTNBENE		:= aDados[nY][13]
					SZ1->Z1_SXBENEF		:= aDados[nY][14]
					SZ1->Z1_NOTSERV		:= aDados[nY][15]
					SZ1->Z1_DTATEND		:= aDados[nY][16]
					SZ1->Z1_DTINTER		:= aDados[nY][17]
					SZ1->Z1_DTALTA		:= aDados[nY][18]
					SZ1->Z1_CDRECEB		:= aDados[nY][19]
					SZ1->Z1_DESGRAU		:= aDados[nY][20]
					SZ1->Z1_EMERG		:= aDados[nY][21]
					SZ1->Z1_HRATEND		:= aDados[nY][22]
					SZ1->Z1_CODACOM		:= aDados[nY][23]
					SZ1->Z1_CODGRAU		:= aDados[nY][24]
					SZ1->Z1_CODSERV		:= aDados[nY][25]
					SZ1->Z1_QTDSERV		:= aDados[nY][26]
					SZ1->Z1_SIGCONS		:= aDados[nY][27]
					SZ1->Z1_NRCONS		:= aDados[nY][28]
					SZ1->Z1_UFCONS		:= aDados[nY][29]
					SZ1->Z1_VLCUSTO		:= aDados[nY][30]
					SZ1->Z1_VLFILME		:= aDados[nY][31]
					SZ1->Z1_VLHMED		:= aDados[nY][32]
					SZ1->Z1_TXADMIN		:= aDados[nY][33]
					SZ1->Z1_VLSERV		:= aDados[nY][34]
					SZ1->Z1_VLBINSS		:= aDados[nY][35]
					SZ1->Z1_MATREMP		:= aDados[nY][36]
					SZ1->Z1_CDPREST		:= aDados[nY][37]
					SZ1->Z1_NMPREST		:= aDados[nY][38]
					SZ1->Z1_ESPEC		:= aDados[nY][39]
					SZ1->Z1_CIDPRIN		:= aDados[nY][40]
					SZ1->Z1_GRAUDEP		:= aDados[nY][41]
					SZ1->Z1_CODPROC		:= aDados[nY][42]
					SZ1->Z1_DESPROC		:= aDados[nY][43]
					SZ1->Z1_RGATEND		:= aDados[nY][44]
					SZ1->Z1_CPF			:= aDados[nY][45]
					SZ1->Z1_DEVCOB		:= aDados[nY][46]
					SZ1->Z1_QTDREAL		:= aDados[nY][47]
					SZ1->Z1_VLTTSER		:= aDados[nY][48]
					SZ1->Z1_VLTSERV		:= aDados[nY][49]
					SZ1->Z1_QTDREG		:= aDados[nY][50]
					SZ1->Z1_DTIMP		:= aDados[nY][51]
					SZ1->Z1_PROCESS		:= "N"
				
				MsUnLock("SZ1")
				
				nTVlImp += aDados[nY][34]
			Next nY
			
			MsgInfo("Foram importados " + cValToChar(nTRImp) + " registros, totalizando R$: " + Transform(nTVlImp, "@E 99,999,999,999.99"),"Conclu�do")

		EndIf
	Else
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