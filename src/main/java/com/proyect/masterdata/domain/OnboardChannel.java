package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOnboardingChannel, schema = Constants.schemaManagement)
public class OnboardChannel {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "onboard_channel_id")
    private Long id;

    @Column(name = "onboard_id", nullable = false)
    private Long onboardId;

    @Column(name = "closing_channel_id", nullable = false)
    private Long closingChannelId;

    @OneToOne
    @JoinColumn(name = "onboard_id", columnDefinition = "onboardId", insertable = false, updatable = false)
    private Onboard onboard;

    @OneToOne
    @JoinColumn(name = "closing_channel_id", columnDefinition = "closingChannelId", insertable = false, updatable = false)
    private ClosingChannel closingChannel;
}
