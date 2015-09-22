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
!Nome 			   ! FIN-PLANOS-AGLUTINA 									 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para a Aglutinação do Contas a Receber dos Planos!
!				   ! de Saúde o Odontológicos								 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 13/02/2015												 !
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
! Programa 	! AglTitPl	! Autor !Gilson Lima 		 ! Data ! 13/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao aglutina titulos gerados dos planos com a competencia	 !
!           ! informada. 													 !
+----------------------------------------------------------------------------*/
User Function AglTitPl()

	Private cPerg		:= "GAGLPLS"
	Private aPergs		:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Data Vencto","","","mv_ch1","D",8,0,0,"G","NAOVAZIO()","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Nova Data Vencto","","","mv_ch2","D",8,0,0,"G","NAOVAZIO()","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Considerar Forma Cobr?","","","mv_ch3","C",1,0,0,"C","","MV_PAR03","Sim","","","1","","Nao","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Forma de Cobrança","","","mv_ch4","C",1,0,0,"C","","MV_PAR04","Financeiro","","","1","","Folha","","","","","Débito Automático","","","","","Boleto","","","","","Fatura","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	If (!Pergunte(cPerg,.T.))
		Return
	EndIf
	
	//Chama função Aglutinação de títulos
	If MsgYesNo("Confirma a Aglutinação dos Títulos?","Atenção")
		Processa({||u_AgTitPl1()}) 
	Endif 
Return
	
/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! AgTitPl1	! Autor !Gilson Lima 		 ! Data ! 13/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao aglutina titulos gerados dos planos com a competencia	 !
!           ! informada. 													 !
+----------------------------------------------------------------------------*/
User Function AgTitPl1(nOrig, dDtV, dNDtV, nCFCob, nFCob)

	/*
		Localizar titulos com mesmo cliente com vencimento na data informada e prefixo PLO e PLS
		com tipos selecionados ou todos
		Adicionar os dados em um Array para aglutinação posterior
		
		// aCliTit[1] - Codigo Cliente
		// aCliTit[2] - Loja Cliente
		// aCliTit[2] - Array de Titulos Cliente (aTitulo) 
	*/
	
	Local aPrefix 		:= {"PLS","PLO"} // Array de Prefixos a considerar
	Local cQuery		:= ''
	Local aTitulo		:= {}
	Local lIncl			:= .F.
	Local nContPref		:= 0
	Local nContTit		:= 0
	Local nContDup		:= 0
	Local cCompet		:= ''
	Local aTitBx		:= {}
	
	DEFAULT nOrig		:= 1 	// 1 = Chamada via perguntas - 2 - Chamada de outra função com passagem de parâmetros

	Private aCliTit		:= {}	// Array de Clientes e Títulos para aglutinação	
	
	Private dDtVenc		:= IIF(nOrig == 1, MV_PAR01, dDtV) 
	Private dNDtVenc	:= IIF(nOrig == 1, MV_PAR02, dNDtV)
	Private nConsCob	:= IIF(nOrig == 1, MV_PAR03, nCFCob)
	Private lTipoCob	:= IIF(nConsCob == 1, .T., .F.)
	Private nForCob		:= IIF(nOrig == 1, MV_PAR04, nFCob)
	Private cTipoCob	:= ''
	Private cNTipoCob	:= ''
	
	Private lMsErroAuto := .F.
	
	// Define o Tipo de Cobrança
	Do Case
		Case nForCob == 1
			cTipoCob	:= 'RC'
		Case nForCob == 2
			cTipoCob	:= 'FOL'
		Case nForCob == 3
			cTipoCob	:= 'DEB'
		Case nForCob == 4
			cTipoCob	:= 'BOL'
		OtherWise
			cTipoCob	:= 'FT'
	EndCase
	
	// Define Competência baseada no novo vencimento do título
	cCompet := Substr(DtoC(dNDtVenc),4,7)
 
	// Busca todos os títulos baseados nos parâmetros
	cQuery := "SELECT SE1.E1_FILIAL, SE1.E1_PREFIXO, SE1.E1_NUM, SE1.E1_PARCELA, SE1.E1_TIPO, SE1.E1_CLIENTE, SE1.E1_LOJA, SE1.E1_VALOR,"
	cQuery += " SE1.E1_EMISSAO, SE1.E1_NATUREZ, SE1.E1_NOMCLI"
	cQuery += " FROM " + RetSqlName("SE1") + " SE1"
	cQuery += " WHERE SE1.D_E_L_E_T_ = ' '"
	cQuery += " AND SE1.E1_STATUS = 'A'"
	cQuery += " AND SE1.E1_VENCTO = '" + DtoS(dDtVenc) + "'"
	
	For nB := 1 To Len(aPrefix)
		nContPref++
		
		If nContPref == 1
			cQuery += " AND ("
		Else
			cQuery += " OR "
		EndIf
		
		cQuery += "SE1.E1_PREFIXO = '" + aPrefix[nB] + "'"
		
		If nContPref == Len(aPrefix)
			cQuery += ")"
		EndIf
	Next nB
	
	If lTipoCob
		cQuery += " AND SE1.E1_TIPO = '" + cTipoCob + "'"
	EndIf
	
	If Select('TMP') <> 0
		dbSelectArea('TMP')
		SZY->(dbCloseArea())
	EndIf
	
	dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TMP',.F.,.F.)
	
	dbSelectArea('TMP')
	TMP->(dbGoTop())
	While TMP->(!EOF())	
	
		nContTit++
	
		aTitulo := Array(11)
		
		aTitulo[1] := TMP->E1_FILIAL
		aTitulo[2] := TMP->E1_PREFIXO
		aTitulo[3] := TMP->E1_NUM
		aTitulo[4] := TMP->E1_PARCELA
		aTitulo[5] := TMP->E1_TIPO
		aTitulo[6] := TMP->E1_CLIENTE
		aTitulo[7] := TMP->E1_LOJA
		aTitulo[8] := TMP->E1_VALOR
		aTitulo[9] := TMP->E1_EMISSAO
		aTitulo[10] := TMP->E1_NATUREZ
		aTitulo[11] := TMP->E1_NOMCLI
			
		lIncl := .F.				
		
		If Len(aCliTit) > 0
					
			For nE := 1 To Len(aCliTit)
				If aCliTit[nE][1] == TMP->E1_CLIENTE .And. aCliTit[nE][2] == TMP->E1_LOJA
					
					aAdd(aCliTit[nE][3],aTitulo) 
					
					lIncl := .T.
					Exit
				EndIf
			Next nE
		EndIf
		If !lIncl
			aAdd(aCliTit,{TMP->E1_CLIENTE, TMP->E1_LOJA,{aTitulo}})
		EndIf
	
		TMP->(dbSkip())
	End
	
	TMP->(dbCloseArea())
	
	// Trata os Títulos Duplicados
	For nF := 1 To Len(aCliTit)
		If Len(aCliTit[nF][3]) > 1
			nContDup++
			
			nNVlTit	:= 0
			
			// Gera novo Título Aglutinado
			
			// Define Tipo do Título
			cNTpTit	:= aCliTit[nF][3][1][5]
			
			// Define Nr. Novo Titulo
			cNNrTit	:= u_DefNrTit(cCompet,'0')

			cNPrefTit	 := 'PLN'				// [PREFIXO] PLN = Títulos de Planos Aglutinados
			cNNatTit	 := '1102001006'		// [NATUREZA] = Mensalidade Saúde
			cNHistTit 	 := 'MENS PLANOS ' + Substr(cCompet,1,2) + "/" + Substr(cCompet,6,2)
			
			// Trata data de Vencimento
			dNDtVencR	:= LastDay(dNDtVenc,3)
			
			// Calcula Total do Título
			For nZ := 1 To Len(aCliTit[nF][3])
				nNVlTit += aCliTit[nF][3][nZ][8]
			Next nZ
			
			// Monta array para ExecAuto
			aNTitulo := {{"E1_PREFIXO",	cNPrefTit,			Nil},;	// Prefixo do Título
						{"E1_NUM",		cNNrTit,			Nil},;	// Número do Título
						{"E1_TIPO",		cNTpTit,			Nil},;	// Tipo do Título
						{"E1_NATUREZ",	cNNatTit,			Nil},;	// Natureza do Título
						{"E1_CLIENTE",	aCliTit[nF][1],		Nil},;	// Código do Titular
						{"E1_LOJA",		aCliTit[nF][2],		Nil},;	// Loja do Titular
						{"E1_EMISSAO",	dDataBase,			Nil},;	// Data de Emissão
						{"E1_VENCTO",	dNDtVenc,			Nil},;	// Data de Vencimento
						{"E1_VENCREA",	dNDtVencR,			Nil},;	// Data de Vencimento Real
						{"E1_VALOR",	nNVlTit,			Nil},;	// Valor do Título
						{"E1_MSFIL",	cFilAnt,			Nil},;	// Filial Corrente
						{"E1_HIST",		cNHistTit,			Nil},;	// Histórico do Título
						{"E1_FATURA",	"NOTFAT",			Nil},;	// Fatura
						{"E1_ORIGEM",	"FINA280",			Nil}}	// Origem

			lMsErroAuto	:= .F.
			
			MSExecAuto({|x,y| Fina040(x,y)},aNTitulo,3) //3- Inclusão, 4- Alteração, 5- Exclusão
			
			If lMsErroAuto
				MostraErro()
				Return()
			EndIf
			
			// Baixa títulos duplicados
			
			For nY := 1 To Len(aCliTit[nF][3])
				
				
				// Adiciona baixa na SE5
				cHistBx := "Bx.Emis.Fat."+cNNrTit
				
				dbSelectArea("SE5")
				
					RecLock("SE5",.T.)
					
						SE5->E5_FILIAL 	:= aCliTit[nF][3][nY][1]
						SE5->E5_DATA 	:= StoD(aCliTit[nF][3][nY][9])
						SE5->E5_TIPO	:= aCliTit[nF][3][nY][5]
						SE5->E5_VALOR	:= aCliTit[nF][3][nY][8]
						SE5->E5_NATUREZ	:= aCliTit[nF][3][nY][10]
						SE5->E5_RECPAG	:= 'R'
						SE5->E5_BENEF	:= aCliTit[nF][3][nY][11]
						SE5->E5_HISTOR	:= cHistBx
						SE5->E5_TIPODOC	:= 'BA'
						SE5->E5_VLMOED2	:= aCliTit[nF][3][nY][8]
						SE5->E5_LA		:= 'S'
						SE5->E5_PREFIXO	:= aCliTit[nF][3][nY][2]
						SE5->E5_NUMERO	:= aCliTit[nF][3][nY][3]
						SE5->E5_CLIFOR	:= aCliTit[nF][3][nY][6]
						SE5->E5_LOJA	:= aCliTit[nF][3][nY][7]
						SE5->E5_DTDIGIT	:= dDataBase
						SE5->E5_MOTBX	:= 'FAT'
						SE5->E5_SEQ		:= '01'
						SE5->E5_DTDISPO	:= dDataBase
						SE5->E5_FILORIG	:= aCliTit[nF][3][nY][1]
						SE5->E5_TXMOEDA	:= 1
						SE5->E5_FATURA	:= cNNrTit
						SE5->E5_FATPREF	:= cNPrefTit
						SE5->E5_SITCOB	:= ' '
						SE5->E5_CLIENTE	:= aCliTit[nF][3][nY][6]
					
					SE5->(MsUnLock())
				SE5->(dbCloseArea())
				
				// Baixa na SE1	
				
				// Filial + Prefixo + Num + Parcela + Tipo
				cChave := aCliTit[nF][3][nY][1] + aCliTit[nF][3][nY][2] + aCliTit[nF][3][nY][3] + aCliTit[nF][3][nY][4] + aCliTit[nF][3][nY][5]
				
				dbSelectArea("SE1")
				SE1->(dbSetOrder(1))
				SE1->(dbGoTop())
				If SE1->(dbSeek(cChave))
					
					RecLock("SE1",.F.)
					
						SE1->E1_BAIXA	:= dDataBase
						SE1->E1_MOVIMEN	:= dDataBase
						SE1->E1_SALDO	:= 0
						SE1->E1_VALLIQ	:= aCliTit[nF][3][nY][8]
						SE1->E1_FATPREF	:= cNPrefTit
						SE1->E1_OK		:= ' '
						SE1->E1_DTFATUR	:= dDataBase
						SE1->E1_STATUS	:= 'B'
						SE1->E1_TIPOFAT	:= cNTpTit
						SE1->E1_FLAGFAT	:= 'S'
						
					SE1->(MsUnLock())
				EndIf
				
				SE1->(dbCloseArea())
				
			Next nY

		EndIf
	Next nF
	
	MsgInfo("Aglutinação de Títulos efetuada com sucesso. " + cValToChar(nContDup) + " Títulos gerados.","Atenção")

Return