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
@Table(name = Constants.tableDistrict, schema = Constants.schemaMaster)
public class District {
        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        @Column(name = "district_id")
        private UUID id;

        @Column(name = "name")
        private String name;

        @Column(name = "status")
        private boolean status;

        @Column(name = "registration_date")
        @CreationTimestamp
        private OffsetDateTime registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private OffsetDateTime updateDate;

        @Column(name = "province_id")
        private UUID provinceId;

        @ManyToOne
        @JoinColumn(name = "province_id", columnDefinition = "provinceId", insertable = false, updatable = false)
        private Province province;

}
