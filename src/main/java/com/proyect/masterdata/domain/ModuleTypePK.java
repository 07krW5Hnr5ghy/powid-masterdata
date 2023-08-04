package com.proyect.masterdata.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

import java.io.Serializable;

@Embeddable
public class ModuleTypePK implements Serializable {
    @Column(name = "id_tipo_usuario_modulo", unique = true)
    private Long idUserTypeModule;

    @Column(name = "id_modulo", unique = true)
    private Long idModule;
}
