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
    @Column(name = "membership_module_id")
    private Long id;

    @Column(name = "membership_id", nullable = false)
    private Long membershipId;

    @Column(name = "module_id", nullable = false)
    private Long moduleId;

    @Column(name = "registration_date", nullable = false)
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date", nullable = false)
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "status", nullable = false)
    private Boolean status;

    @ManyToOne
    @JoinColumn(name = "membership_id", columnDefinition = "membershipId", insertable = false, updatable = false)
    private Membership membership;

    @ManyToOne
    @JoinColumn(name = "module_id", columnDefinition = "moduleId", insertable = false, updatable = false)
    private Module module;

}
