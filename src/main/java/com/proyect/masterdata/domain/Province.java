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
@Table(name = Constants.tableProvince, schema = Constants.schemaMaster)
public class Province {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "province_id")
        private Long id;

        @Column(name = "name", nullable = false)
        private String name;

        @Column(name = "status", nullable = false)
        private Boolean status;

        @Column(name = "registration_date", nullable = false)
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "department_id", nullable = false)
        private Long departmentId;

        @Column(name = "token_user", nullable = false)
        private String tokenUser;

        @ManyToOne
        @JoinColumn(name = "department_id", columnDefinition = "departmentId", insertable = false, updatable = false)
        private Department department;

}
