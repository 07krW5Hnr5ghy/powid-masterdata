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

import java.time.OffsetDateTime;
import java.util.UUID;

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
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "membership_module_id")
    private UUID id;

    @Column(name = "membership_id", nullable = false)
    private Long membershipId;

    @Column(name = "module_id", nullable = false)
    private Long moduleId;

    @Column(name = "registration_date", nullable = false)
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date", nullable = false)
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "status", nullable = false)
    private Boolean status;

    @ManyToOne
    @JoinColumn(name = "membership_id", columnDefinition = "membershipId", insertable = false, updatable = false)
    private Membership membership;

    @ManyToOne
    @JoinColumn(name = "module_id", columnDefinition = "moduleId", insertable = false, updatable = false)
    private Module module;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne()
    @Column(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

}
