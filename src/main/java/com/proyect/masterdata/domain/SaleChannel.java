package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.OffsetDateTime;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableSaleChannel, schema = Constants.schemaMaster)
public class SaleChannel {
        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        @Column(name = "sale_channel_id")
        private String id;

        @Column(name = "name", nullable = false)
        private String name;

        @Column(name = "status", nullable = false)
        private Boolean status;

        @Column(name = "registration_date")
        @CreationTimestamp
        private OffsetDateTime registrationDate;

        @Column(name = "user_id")
        private String userId;

        @ManyToOne()
        @Column(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
        private User user;
}
