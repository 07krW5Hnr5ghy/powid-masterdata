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
@Table(name = Constants.tableDistrict, schema = Constants.schemaMaster)
public class District {
        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "district_id")
        private Long id;

        @Column(name = "name", length = 50)
        private String name;

        @Column(name = "status")
        private boolean status;

        @Column(name = "registration_date")
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "province_id")
        private Long provinceId;

        @ManyToOne
        @JoinColumn(name = "province_id", columnDefinition = "provinceId", insertable = false, updatable = false)
        private Province province;

        @Column(name = "token_user")
        private String tokenUser;

}
