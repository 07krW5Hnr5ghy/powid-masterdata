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
@Table(name = Constants.tableProductPicture,schema = Constants.schemaMarketing)
public class ProductPicture {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "product_picture_id")
    private String id;

    @Column(name = "product_id")
    private Long productId;

    @Column(name = "product_picture_url")
    private String productPictureUrl;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "token_user")
    private String tokenUser;

    @Column(name = "client_id")
    private Long clientId;

    @ManyToOne
    @JoinColumn(name = "product_id",columnDefinition = "productId",insertable = false,updatable = false)
    private Product product;

    @ManyToOne
    @JoinColumn(name = "client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;
}
