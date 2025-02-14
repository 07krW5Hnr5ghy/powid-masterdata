package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.OffsetDateTime;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableModule, schema = Constants.schemaMaster)
public class Module {

        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        @Column(name = "module_id")
        private String id;

        @Column(name = "name", nullable = false)
        private String name;

        @Column(name = "monthly_price", nullable = false)
        private Double monthlyPrice;

        @Column(name = "status", nullable = false)
        private Boolean status;

        @Column(name = "registration_date", nullable = false)
        @CreationTimestamp
        private OffsetDateTime registrationDate;

        @Column(name = "update_date", nullable = false)
        @CreationTimestamp
        private OffsetDateTime updateDate;

        @Column(name = "token_user", nullable = false)
        private String tokenUser;

}
