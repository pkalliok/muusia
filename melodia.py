
muusat = {}

def uus_muus(muusa): muusat[muusa.nimi] = muusa

iskujen_jako = 48

class Tilanne: pass

def oletustilanne():
    t = Tilanne()
    t.__dict__.update(
        alku = 0,
        pituus = 4,
        aikasiirtymä = 0,
        aikamittakaava = iskujen_jako,
        keskeytykset = [],
        soittimet = {},
        sävel = 64,
        painotus = 100,
    )
    return t

def uusi_tilanne(tilanne, **muutokset):
    t = Tilanne()
    t.__dict__.update(tilanne.__dict__)
    t.__dict__.update(muutokset)
    return t

def teos(nimi, sisältö):
    teos_tapahtumat = sorted(sisältö(oletustilanne()))
    teos_loppuu = max(tapahtuma[1] for tapahtuma in teos_tapahtumat) + 96
    return [
            (0, 0, "Header", 0, 1, iskujen_jako),
            (1, 0, "Start_track"),
            (1, 0, "Title_t", nimi),
            (1, 0, "Tempo", 500000),
        ] + teos_tapahtumat + [
            (1, teos_loppuu, "End_track"),
            (0, 0, "End_of_file"),
        ]

def tapahtuma(*args): return [(1,) + args]

def alku(tilanne):
    return (tilanne.alku + tilanne.aikasiirtymä) * tilanne.aikamittakaava

def loppu(tilanne):
    return (tilanne.alku + tilanne.aikasiirtymä
            + tilanne.pituus) * tilanne.aikamittakaava

def soitettavissa(tilanne):
    return not any(k_alku <= tilanne.alku < k_loppu
            for k_alku, k_loppu in tilanne.keskeytykset)

def varaa_kanava(tilanne, soitin):
    try: return (tilanne.soittimet[soitin], [])
    except KeyError:
        varatut_kanavat = set(tilanne.soittimet.values())
        kanava = next(ch for ch in range(16) if ch not in varatut_kanavat)
        tilanne.soittimet[soitin] = kanava
        pankki, ohjelma = soitin
        return (kanava,
            tapahtuma(alku(tilanne), "Control_c", kanava, 0, pankki // 128)
            + tapahtuma(alku(tilanne), "Control_c", kanava, 32, pankki % 128)
            + tapahtuma(alku(tilanne), "Program_c", kanava, ohjelma))

def nuotti(tilanne, soitin):
    if not soitettavissa(tilanne): return []
    (kanava, soittimenvaihto) = varaa_kanava(tilanne, soitin)
    return (soittimenvaihto
            + tapahtuma(alku(tilanne), "Note_on_c",
                kanava, tilanne.sävel, tilanne.painotus)
            + tapahtuma(loppu(tilanne), "Note_off_c",
                kanava, tilanne.sävel, tilanne.painotus))

def midisoitin(nimi, pankki, ohjelma):
    def soita(tilanne):
        return nuotti(tilanne, (pankki, ohjelma))
    soita.nimi = nimi
    return soita

