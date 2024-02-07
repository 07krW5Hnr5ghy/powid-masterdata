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
@Table(name = Constants.tableProductPicture,schema = Constants.schemaArticle)
public class ProductPicture {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_producto_imagen")
    private Long id;

    @Column(name = "id_producto")
    private Long productId;

    @Column(name = "url_producto_imagen")
    private String productPictureUrl;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "usuario_token")
    private String tokenUser;

    @Column(name = "id_cliente")
    private Long clientId;

    @ManyToOne
    @JoinColumn(name = "id_producto",columnDefinition = "productId",insertable = false,updatable = false)
    private Product product;

    @ManyToOne
    @JoinColumn(name = "id_cliente",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;
}
