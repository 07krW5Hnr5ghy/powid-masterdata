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
@Table(name = Constants.tableCategory, schema = Constants.schemaMaster)
public class Category {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "category_id", unique = true)
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

        @Column(name = "token_user")
        private String tokenUser;
}
