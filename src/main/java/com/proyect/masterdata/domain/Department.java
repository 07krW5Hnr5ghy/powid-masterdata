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
@Table(name = Constants.tableDepartment, schema = Constants.schemaMaster)
public class Department {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "department_id")
        private Long id;

        @Column(name = "name", length = 50)
        private String name;

        @Column(name = "status")
        private Boolean status;

        @Column(name = "registration_date")
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "token_user")
        private String tokenUser;

}
