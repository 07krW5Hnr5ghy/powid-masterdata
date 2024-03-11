package com.proyect.masterdata.domain;

import java.util.Date;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
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
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "role_access_id")
    private Long id;

    @Column(name = "role_id", nullable = false)
    private Long roleId;

    @Column(name = "access_id", nullable = false)
    private Long accessId;

    @Column(name = "token_user", nullable = false)
    private String tokenUser;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "status")
    private Boolean status;
}
