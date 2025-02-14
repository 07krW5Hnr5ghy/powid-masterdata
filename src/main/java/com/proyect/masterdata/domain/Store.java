package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;

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
        private String id;

        @Column(name = "name", nullable = false)
        private String name;

        @Column(name = "url", nullable = false)
        private String url;

        @Column(name = "status", nullable = false)
        private Boolean status;

        @Column(name = "registration_date")
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "client_id", nullable = false)
        private Long clientId;

        @Column(name = "token_user", nullable = false)
        private String tokenUser;

        @Column(name = "store_type_id", nullable = false)
        private Long storeTypeId;

        @OneToOne
        @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
        private Client client;

        @ManyToOne
        @JoinColumn(name = "store_type_id", columnDefinition = "storeTypeId", insertable = false, updatable = false)
        private StoreType storeType;
}
