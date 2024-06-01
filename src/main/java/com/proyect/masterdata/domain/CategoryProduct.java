package com.proyect.masterdata.domain;

import java.util.Date;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableCategoryProduct, schema = Constants.schemaMaster)
public class CategoryProduct {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "category_product_id", unique = true)
    private Long id;

    @Column(name = "name", length = 50)
    private String name;

    @Column(name = "description", length = 50)
    private String description;

    @Column(name = "status", columnDefinition = "BOOLEAN DEFAULT TRUE")
    private Boolean status;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "size_type_id")
    private Long sizeTypeId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "size_type_id", columnDefinition = "sizeTypeId", insertable = false, updatable = false)
    private SizeType sizeType;
}
