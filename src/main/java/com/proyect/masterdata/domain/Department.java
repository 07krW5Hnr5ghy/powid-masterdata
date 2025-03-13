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
@Table(name = Constants.tableDepartment, schema = Constants.schemaMaster)
public class Department {

        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        @Column(name = "department_id")
        private UUID id;

        @Column(name = "name")
        private String name;

        @Column(name = "status")
        private Boolean status;

        @Column(name = "registration_date")
        @CreationTimestamp
        private OffsetDateTime registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private OffsetDateTime updateDate;

        @Column(name = "country_id")
        private UUID countryId;

        @ManyToOne
        @JoinColumn(name = "country_id", columnDefinition = "countryId", insertable = false, updatable = false)
        private Country country;

}
