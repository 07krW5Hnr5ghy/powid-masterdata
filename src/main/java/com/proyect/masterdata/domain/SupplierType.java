package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableSupplierType, schema = Constants.schemaLogistics)
public class SupplierType {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "supplier_type_id")
    private UUID id;
}
