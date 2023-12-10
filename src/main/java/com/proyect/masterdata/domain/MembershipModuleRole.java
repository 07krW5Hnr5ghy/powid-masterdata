package com.proyect.masterdata.domain;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Table(name = Constants.tableMembershipModuleRole, schema = Constants.schemaManagement)
public class MembershipModuleRole {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_membresia_modulo_rol")
    private Long id;

    @Column(name = "id_membresia_modulo", nullable = false)
    private Long membershipModuleId;

    @Column(name = "id_role", nullable = false)
    private Long roleId;

    @Column(name = "fecha_registro", nullable = false)
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    private Date updateDate;

    @Column(name = "estado", nullable = false)
    private Boolean status;

    @OneToOne
    @JoinColumn(name = "id_membresia_modulo", columnDefinition = "membershipModuleId", insertable = false, updatable = false)
    private MembershipModule membershipModule;

    @ManyToOne
    @JoinColumn(name = "id_role", columnDefinition = "roleId", insertable = false, updatable = false)
    private Role role;

}
