package com.proyect.masterdata.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableMembershipModule, schema = Constants.schemaManagement)
public class MembershipModule {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_membresia_modulo")
    private Long id;

    @Column(name = "id_membership", nullable = false)
    private Long membershipId;

    @Column(name = "id_modulo", nullable = false)
    private Long moduleId;

    @Column(name = "fecha_registro", nullable = false)
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion", nullable = false)
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "estado", nullable = false)
    private Boolean status;

    @ManyToOne
    @JoinColumn(name = "id_membership", columnDefinition = "membershipId", insertable = false, updatable = false)
    private Membership membership;

    @ManyToOne
    @JoinColumn(name = "id_modulo", columnDefinition = "moduleId", insertable = false, updatable = false)
    private Module module;

}
