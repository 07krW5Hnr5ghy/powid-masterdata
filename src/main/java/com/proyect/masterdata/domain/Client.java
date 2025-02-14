package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableClient, schema = Constants.schemaManagement)
public class Client {

        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        @Column(name = "client_id")
        private UUID id;

        @Column(name = "name", nullable = false)
        private String name;

        @Column(name = "surname", nullable = false)
        private String surname;

        @Column(name = "ruc", nullable = false)
        private String ruc;

        @Column(name = "dni", nullable = false)
        private String dni;

        @Column(name = "business", nullable = false)
        private String business;

        @Column(name = "mobile", nullable = false)
        private String mobile;

        @Column(name = "address", nullable = false)
        private String address;

        @Column(name = "email", nullable = false)
        private String email;

        @Column(name = "status", nullable = false)
        private Boolean status;

        @Column(name = "district_id", nullable = false)
        private UUID districtId;

        @Column(name = "registration_date")
        @CreationTimestamp
        private OffsetDateTime registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private OffsetDateTime updateDate;

        @ManyToOne
        @JoinColumn(name = "district_id", columnDefinition = "districtId", insertable = false, updatable = false)
        private District district;

}
