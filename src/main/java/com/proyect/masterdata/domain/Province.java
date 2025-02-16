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
@Table(name = Constants.tableProvince, schema = Constants.schemaMaster)
public class Province {

        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        @Column(name = "province_id")
        private UUID id;

        @Column(name = "name", nullable = false)
        private String name;

        @Column(name = "status", nullable = false)
        private Boolean status;

        @Column(name = "registration_date", nullable = false)
        @CreationTimestamp
        private OffsetDateTime registrationDate;

        @Column(name = "update_date", nullable = false)
        @CreationTimestamp
        private OffsetDateTime updateDate;

        @Column(name = "department_id", nullable = false)
        private UUID departmentId;

        @ManyToOne
        @JoinColumn(name = "department_id", columnDefinition = "departmentId", insertable = false, updatable = false)
        private Department department;

}
