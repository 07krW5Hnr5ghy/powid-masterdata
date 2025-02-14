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
@Table(name = Constants.tableOrderReturnType, schema = Constants.schemaMaster)
public class OrderReturnType {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "order_return_type_id")
    private String id;

    @Column(name = "name")
    private String name;

    @Column(name = "registration_date")
    @CreationTimestamp()
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp()
    private Date updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "tokenUser")
    private String tokenUser;
}
