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
    @Column(name = "membership_module_role_id")
    private Long id;

    @Column(name = "membership_module_id", nullable = false)
    private Long membershipModuleId;

    @Column(name = "role_id", nullable = false)
    private Long roleId;

    @Column(name = "registration_date", nullable = false)
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    private Date updateDate;

    @Column(name = "status", nullable = false)
    private Boolean status;

    @OneToOne
    @JoinColumn(name = "membership_module_id", columnDefinition = "membershipModuleId", insertable = false, updatable = false)
    private MembershipModule membershipModule;

    @ManyToOne
    @JoinColumn(name = "role_id", columnDefinition = "roleId", insertable = false, updatable = false)
    private Role role;

}
