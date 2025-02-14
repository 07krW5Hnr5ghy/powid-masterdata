package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableSizeType, schema = Constants.schemaMaster)
public class SizeType {

        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        @Column(name = "size_type_id")
        private UUID id;

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

        @Column(name = "user_id")
        private UUID userId;

        @ManyToOne()
        @Column(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
        private User user;
}
