User Function AtVlAquis()
dbSelectArea('SN3')
SN3->(dbSetOrder(1))

While SN3->(!Eof())

	dbSelectArea('SN1')
	SN1->(dbSetOrder(1))
	SN1->(dbGoTop())
	
	If SN1->(dbSeek(xFilial('SN1')+SN3->N3_CBASE+SN3->N3_ITEM))
		RecLock('SN1',.F.)
			SN1->N1_VLAQUIS := SN3->N3_VORIG1
		MsUnlock()
	EndIf
	
	SN1->(dbCloseArea())

	SN3->(dbSkip())
EndDo

SN3->(dbCloseArea())

Alert('Atualização Concluída!')
Return