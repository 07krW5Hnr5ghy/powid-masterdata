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
@Table(name = Constants.tableModule, schema = Constants.schemaMaster)
public class Module {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "module_id")
        private Long id;

        @Column(name = "name", nullable = false)
        private String name;

        @Column(name = "monthly_price", nullable = false)
        private Double monthlyPrice;

        @Column(name = "status", nullable = false)
        private Boolean status;

        @Column(name = "registration_date", nullable = false)
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "update_date", nullable = false)
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "token_user", nullable = false)
        private String tokenUser;

}
