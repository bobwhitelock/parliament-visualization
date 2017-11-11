
require 'sinatra'
require 'sequel'

DB = Sequel.postgres(username: 'postgres')


get '/votes' do
  latest_vote = DB[:votes].order(:date).last
  latest_vote_with_events = latest_vote.merge(
    voteEvents: vote_events_for(latest_vote)
  )

  {
    latestVote: latest_vote_with_events,
    votes: DB[:votes].to_a
  }.to_json
end

get '/vote-events/:vote_id' do |vote_id|
  vote = DB[:votes][{id: vote_id}]
  vote_events_for(vote).to_json
end


def vote_events_for(vote)
  DB[:vote_events].where(
    vote_id: vote[:id]
  ).join(
    :people,
    person_id: :person_id,
    date: vote[:date]
  ).to_a
end
