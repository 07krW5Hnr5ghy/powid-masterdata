package com.proyect.masterdata.domain;

import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.*;
import lombok.Builder;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableAccess, schema = Constants.schemaManagement)
public class Access {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "access_id")
    private String id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "status", nullable = false)
    private Boolean status;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name="user_id")
    private String userId;

    @ManyToOne()
    @Column(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;
}
