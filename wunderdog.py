#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import re

kahva = open(sys.argv[1], 'r')
aineisto = kahva.read().decode('utf8')
kahva.close()
#sanat = re.split(' |\'|:|.|\r|\n|,|?|;|!|(|)|"|_|/', aineisto)
sanat = re.split(' |\'|\:|\.|\r|\n|\,|\?|\;|\!|\(|\)|\"|\_|\/', aineisto)

def laske_pisteet(sana):
    vokaaliKetjut = re.findall(u'[qeuioåaöä]+', sana)
    return sum([len(k) * (2 ** len(k)) for k in vokaaliKetjut])

korkein = 0
johto = []
for sana in sanat:
    pisteet = laske_pisteet(sana)
    if pisteet > korkein:
        korkein = pisteet
        johto = [sana]
    elif pisteet == korkein:
        johto.append(sana)

sanaLause = u'Hassuimmat sanat olivat: ' + u', '.join(sorted(johto))
pisteLause =  u'Nämä saivat ' + unicode(korkein) + u' pistettä.'
rivit = [sanaLause, pisteLause]
vastaus = u'\n'.join(rivit)
print vastaus.encode('utf8')
