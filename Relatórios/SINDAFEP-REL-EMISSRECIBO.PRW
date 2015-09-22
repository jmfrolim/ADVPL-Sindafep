/*
+----------------------------------------------------------------------------+
!                         FICHA TECNICA DO PROGRAMA                          !
+----------------------------------------------------------------------------+
!   DADOS DO PROGRAMA                                                        !
+------------------+---------------------------------------------------------+
!Tipo              ! Relatorio                                               !
+------------------+---------------------------------------------------------+
!Modulo            ! FIN                                                     !
+------------------+---------------------------------------------------------+
!Nome              ! RFIN002                                                 !
+------------------+---------------------------------------------------------+
!Descricao         ! Relatório de emissão de recibos.                        !
!                  !                                                         !
+------------------+---------------------------------------------------------+
!Autor             ! Marcos Vinicius Perusselo                               !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 14/04/2015                                              !
+------------------+---------------------------------------------------------+
!   ATUALIZACOES                                                             !
+-------------------------------------------+-----------+-----------+--------+
!   Descricao detalhada da atualizacao      !Nome do    ! Analista  !Data da !
!                                           !Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
!                                           !           !           !        !
!                                           !           !           !        !
+-------------------------------------------+-----------+-----------+--------+
*/

#INCLUDE "PROTHEUS.CH"
#INCLUDE "RWMAKE.CH"

#DEFINE CRLF CHR(13) + CHR(10)

User Function RFIN002()
	Local cChave := SE1->E1_PREFIXO+SE1->E1_NUM+SE1->E1_PARCELA
	Private cPerg := "RFIN002"
	Private aPergs := {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	
	If (SX1->(!Found()))
		AADD(aPergs,{"Data Emissão de?","","","mv_ch1","D",8,0,0,"G","","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Data Emissão até?","","","mv_ch2","D",8,0,0,"G","","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Série de?","","","mv_ch3","C",2,0,0,"G","","MV_PAR03","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Série até?","","","mv_ch4","C",2,0,0,"G","","MV_PAR04","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Recibo de?","","","mv_ch5","C",6,0,0,"G","","MV_PAR05","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Recibo até?","","","mv_ch6","C",6,0,0,"G","","MV_PAR06","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Imprim. Apenas Cancelad.?","","","mv_ch7","N",1,0,1,"C","","MV_PAR07","Não","Não","Não","","","Sim","Sim","Sim","","","","","","","","","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	If (!Pergunte(cPerg,.T.))
		Return
	EndIf
	
	Processa({|| RFIN002A()}, "Gerando Relatório")
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa  ! RFIN002A  ! Autor !Marcos V Perusselo  ! Data ! 14/04/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao !                                                                !
!           !                                                                !
+----------------------------------------------------------------------------*/
Static Function RFIN002A()
	Local oPrint := Nil
	Local cAliasA := GetNextAlias()
	Local dDtEmiDe := MV_PAR01
	Local dDtEmiAte := MV_PAR02
	Local cSerieDe := MV_PAR03
	Local cSerieAte := MV_PAR04
	Local cRecibDe := MV_PAR05
	Local cRecibAte := MV_PAR06
	Local nRecibCan := MV_PAR07
	Local nQntReg := 0
	Local nQntEnt := 0
	Local nQntSai := 0
	Local nRow := 0
	Local nRowAux := 0
	Local nRowAdd := 0
	Local nAcomAux := 0
	Local nTotEnt := 0
	Local nTotSai := 0
	Local nQtdPag := 0
	Local nRegPag := 46
	Local nTotReg := 0
	Local nPagAtu := 0
	
	Private oFont8		:= TFont():New("Arial",9,8,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont8b		:= TFont():New("Arial",9,8,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont8m		:= TFont():New("Courier New",9,8,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont8mb	:= TFont():New("Courier New",9,8,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont9		:= TFont():New("Arial",9,9,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont9b		:= TFont():New("Arial",9,9,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont9m		:= TFont():New("Courier New",9,9,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont9mb	:= TFont():New("Courier New",9,9,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont10		:= TFont():New("Arial",9,10,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont10b	:= TFont():New("Arial",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont10m	:= TFont():New("Courier New",9,10,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont10mb	:= TFont():New("Courier New",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont12		:= TFont():New("Arial",9,12,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont12b	:= TFont():New("Arial",9,12,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont12m	:= TFont():New("Courier New",9,12,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont12mb	:= TFont():New("Courier New",9,12,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont14		:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont14b	:= TFont():New("Arial",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont14m	:= TFont():New("Courier New",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont14mb	:= TFont():New("Courier New",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont16b	:= TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont18b	:= TFont():New("Arial",9,18,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont18mb	:= TFont():New("Courier New",9,18,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont20mb	:= TFont():New("Courier New",9,20,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont24b	:= TFont():New("Arial",9,24,.T.,.T.,5,.T.,5,.T.,.F.)
	
	BeginSQL Alias cAliasA
		SELECT *
		FROM %Table:SZR%
		WHERE
		ZR_FILIAL = %xFilial:SZR%
		AND (ZR_NUM >= %Exp:cRecibDe% AND ZR_NUM <= %Exp:cRecibAte%)
		AND (ZR_SERIE >= %Exp:cSerieDe% AND ZR_SERIE <= %Exp:cSerieAte%)
		AND (ZR_DTEMISS >= %Exp:DtoS(dDtEmiDe)% AND ZR_DTEMISS <= %Exp:DtoS(dDtEmiAte)%)
		AND (ZR_STATUS = %Exp:nRecibCan%)
		AND %NotDel%
	EndSQL
	
	dbSelectArea(cAliasA)
	(cAliasA)->(dbGoTop())
	
	(cAliasA)->(dbEval({|| nTotReg++}))
	
	nQtdPag := Ceiling(nTotReg / nRegPag)
	
	(cAliasA)->(dbGoTop())
	
	oPrint:= TMSPrinter():New("Emissao Recibos Sindafep")
	oPrint:Setup()
	oPrint:SetPaperSize(9)
	oPrint:SetPortrait()
	
	While ((cAliasA)->(!EOF()))
		nRow := 0
		nRowAux := 0
		nRowAdd := 50
		nQntReg := 0
		
		oPrint:StartPage()
		
		nPagAtu++
		
		oPrint:Say(nRow+030,0650,"Relatório de Emissão de Recibos", oFont18b)
		
		oPrint:Line(nRow+100,150,nRow+100,2000)
		
		oPrint:Say(nRow+120,0230,"Emitidos no período de " + DtoC(dDtEmiDe) + " a " + DtoC(dDtEmiAte), oFont14b)
		oPrint:Say(nRow+120,1800,SubStr(Time(), 1, 5), oFont14b)
		oPrint:Say(nRow+090,2100,"Pág.: " + cValToChar(nPagAtu), oFont10b)
		
		oPrint:Say(nRow+220,0010,"Rec./Série", oFont12b)
		oPrint:Say(nRow+220,0240,"RG/CNPJ", oFont12b)
		oPrint:Say(nRow+220,0490,"Acomodação", oFont12b)
		//oPrint:Say(nRow+220,1420,"Data", oFont12b)
		//oPrint:Say(nRow+260,1320,"Entrada", oFont10b)
		oPrint:Say(nRow+260,1520,"Dt. Emiss.", oFont10b)
		oPrint:Say(nRow+220,1780,"Valor Recibo", oFont12b)
		oPrint:Say(nRow+260,1750,"Entrada", oFont10b)
		oPrint:Say(nRow+260,1940,"Saída", oFont10b)
		
		oPrint:Line(nRow+320,0010,nRow+320,2250)
		
		While (nQntReg < nRegPag .AND. nRow < 2650 .AND. (cAliasA)->(!EOF()))
			nRowAux := 0
			nRowAdd := 50
			nAcomAux := 0
			cAcomod := AllTrim(AllTrim((cAliasA)->ZR_DESCR1) + " " + AllTrim((cAliasA)->ZR_DESCR2) + " " + AllTrim((cAliasA)->ZR_DESCR3) + " " + AllTrim((cAliasA)->ZR_DESCR4) + " " + AllTrim((cAliasA)->ZR_DESCR5))
			
			oPrint:Say(nRow+350,0010,AllTrim((cAliasA)->ZR_NUM) + "-" + AllTrim((cAliasA)->ZR_SERIE), oFont8b) // Recibo
			oPrint:Say(nRow+350,0240,AllTrim((cAliasA)->ZR_TITNUM), oFont8b) // RG
			
			If (Len(cAcomod) > 55)
				While (Len(cAcomod) > 55)
					oPrint:Say(nRow+350+nRowAux,0490,SubStr(cAcomod, nAcomAux+1, nAcomAux+55), oFont8b) // Acomodacao (48)
					cAcomod := SubStr(cAcomod, nAcomAux+56, Len(cAcomod))
					nRowAdd += 50
					nRowAux += 50
					nAcomAux += 55
				EndDo
				
				oPrint:Say(nRow+350+nRowAux,0490,cAcomod, oFont8b) // Acomodacao (48)
			Else
				oPrint:Say(nRow+350,0490,cAcomod, oFont8b) // Acomodacao (48)
			EndIf
			
			//oPrint:Say(nRow+350,1320,"22/01/2015", oFont8b) // Data Entrada
			oPrint:Say(nRow+350,1520,DtoC(StoD(AllTrim((cAliasA)->ZR_DTEMISS))), oFont8b) // Data Emiss.
			
			If (AllTrim((cAliasA)->ZR_TIPO) != "NCC")
				oPrint:Say(nRow+350,1750,AllTrim(Transform((cAliasA)->ZR_TITVL, "@E 999,999,999.99")), oFont8b) // Entrada
				nQntEnt++
				nTotEnt += (cAliasA)->ZR_TITVL
			Else
				oPrint:Say(nRow+350,1940,AllTrim(Transform((cAliasA)->ZR_TITVL, "@E 999,999,999.99")), oFont8b) // Saida
				nQntSai++
				nTotSai += (cAliasA)->ZR_TITVL
			EndIf
			
			//oPrint:Say(nRow+350,2100,IIF(AllTrim((cAliasA)->ZR_FPAGTO) != "CHEQUE", "SEM CAUÇÃO", ""), oFont8b) // Caucao
			
			nRow += nRowAdd
			
			(cAliasA)->(dbSkip())
			nQntReg++
		EndDo
		
		If (nPagAtu == nQtdPag)
			oPrint:Line(nRow+375,0010,nRow+375,2250)
			
			oPrint:Say(nRow+400,1750,AllTrim(Transform(nTotEnt, "@E 999,999,999.99")), oFont10b) // Entrada
			oPrint:Say(nRow+400,1940,AllTrim(Transform(nTotSai, "@E 999,999,999.99")), oFont10b) // Saida
			
			oPrint:Say(nRow+500,1520,"Recibos emitidos: " + cValToChar(nTotReg), oFont12b)
			oPrint:Say(nRow+550,1520,"Entradas: " + cValToChar(nQntEnt), oFont12b)
			oPrint:Say(nRow+600,1520,"Saídas: " + cValToChar(nQntSai), oFont12b)
		EndIf
		
		oPrint:EndPage()
	EndDo
	
	oPrint:Preview()
	
	(cAliasA)->(dbCloseArea())
Return