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
@Table(name = Constants.tableStore, schema = Constants.schemaManagement)
public class Store {

        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        @Column(name = "store_id", unique = true)
        private UUID id;

        @Column(name = "name", nullable = false)
        private String name;

        @Column(name = "url", nullable = false)
        private String url;

        @Column(name = "status", nullable = false)
        private Boolean status;

        @Column(name = "registration_date")
        @CreationTimestamp
        private OffsetDateTime registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private OffsetDateTime updateDate;

        @Column(name = "client_id", nullable = false)
        private UUID clientId;

        @Column(name = "user_id")
        private UUID userId;

        @Column(name = "store_type_id", nullable = false)
        private UUID storeTypeId;

        @OneToOne
        @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
        private Client client;

        @ManyToOne
        @JoinColumn(name = "store_type_id", columnDefinition = "storeTypeId", insertable = false, updatable = false)
        private StoreType storeType;

        @ManyToOne()
        @Column(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
        private User user;
}
