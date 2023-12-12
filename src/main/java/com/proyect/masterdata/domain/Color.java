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
@Table(name = Constants.tableColor, schema = Constants.schemaMaster)
public class Color {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_color")
        private Long id;

        @Column(name = "nombre", length = 50)
        private String name;

        @Column(name = "estado")
        private boolean status;

        @Column(name = "fecha_registro")
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "fecha_modificacion")
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "usuario")
        private String tokenUser;
}
