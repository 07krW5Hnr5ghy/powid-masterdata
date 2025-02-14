package com.proyect.masterdata.domain;

import java.time.OffsetDateTime;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableRoleAccess, schema = Constants.schemaManagement)
public class RoleAccess {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "role_access_id")
    private String id;

    @Column(name = "role_id", nullable = false)
    private Long roleId;

    @Column(name = "access_id", nullable = false)
    private Long accessId;

    @Column(name = "token_user", nullable = false)
    private String tokenUser;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "status")
    private Boolean status;

    @ManyToOne
    @JoinColumn(name = "role_id", columnDefinition = "roleId", insertable = false, updatable = false)
    private Role role;

    @ManyToOne
    @JoinColumn(name = "access_id", columnDefinition = "accessId", insertable = false, updatable = false)
    private Access access;
}
