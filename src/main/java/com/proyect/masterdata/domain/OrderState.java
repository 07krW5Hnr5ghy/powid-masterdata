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
@Table(name = Constants.tableOrderState, schema = Constants.schemaMaster)
public class OrderState {

        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        @Column(name = "order_state_id")
        private String id;

        @Column(name = "name", nullable = false)
        private String name;

        @Column(name = "hex_color",nullable = false)
        private String hexColor;

        @Column(name = "status", nullable = false)
        private Boolean status;

        @Column(name = "registration_date")
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "token_user", nullable = false)
        private String tokenUser;
}
